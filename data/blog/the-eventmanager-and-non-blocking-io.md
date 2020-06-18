# How the GHC Utilizes Non-Blocking IO

GHC RTS utilizes non-blocking IO efficently while providing blocking IO semantics.

In this way Haskell programmers get the best both worlds. They can write code in straight forward way as though the IO calls have blocking semantics while they get the performance of non-blocking IO using a pool of worker OS threads.

## A Quick Tour of the `EventManager`

The key to implementing non-blocking IO in GHC starts with the `EventManager`.

The `EventManager` has a very simple API. I want to focus on the most important function.

```haskell
registerFd :: EventManager -> IOCallback -> Fd -> Event -> Lifetime -> IO FdKey
```

This function lets one express interest in file descriptor ready events. For instance one can register a callback that will get called when a file descriptor is ready for reading:

```haskell
registerRead fd = do
  Just ev <- getSystemEventManager
  let callback fdKey event = when (event == evtRead) $ putStrLn "Read Callback"
  registerFd ev callback fd evtRead OneShot
```

Using the function above "Read Callback" will get printed when there is data to read on a file descriptor. You can ignore the `OneShot` part. The event manager only supports `OneShot` callback semantics.

This is a neat trick but is critically important when working with non-blocking file descriptors. Consider the POSIX function `recvfrom` which is to used to recieve bytes on a socket.

In typical blocking IO usage, a call to `recvfrom` on an empty socket with block the thread until data arrives in the socket recieve buffer. This not a problem by itself. However if one naively tried to make a server which used a separate thread for every connection it would be an inefficent use of OS threads.

Non-blocking IO is useful because it faciliates a pattern of servicing a large number of sockets using a much smaller pool of threads. When one calls `recvfrom` on a non-blocking empty socket it returns `-1` and sets the `errno` to `EWOULDBLOCK`.

The OS is essentially telling us the socket is empty and we should call `recvfrom` later. We have a few options to call it again. We can poll the socket but doing so is inefficent or introduces unnecessary latency or both. A much better option is to register a callback to call when the socket is ready for reading.

Registering interest in ready events is one piece of implementing non-blocking IO efficently but we still need a way to schedule the work on our pool of worker threads.

Luckily for the `EventManager` implementators GHC already had a preemptive scheduler utilizing lightweight green threads. The scheduler takes care of running Haskell code on OS threads.

To utilize the GHC green threads we need a function like `threadWaitSTM`

```haskell
threadWaitSTM :: Event -> Fd -> IO (STM (), IO ())
threadWaitSTM evt fd = mask_ $ do
  m <- newTVarIO Nothing
  mgr <- getSystemEventManager_
  reg <- registerFd mgr (\_ e -> atomically (writeTVar m (Just e))) fd evt M.OneShot
  let waitAction =
        do mevt <- readTVar m
           case mevt of
             Nothing -> retry
             Just evt' ->
               if evt' `eventIs` evtClose
               then throwSTM $ errnoToIOError "threadWaitSTM" eBADF Nothing Nothing
               else return ()
  return (waitAction, unregisterFd_ mgr reg >> return ())
```

`threadWaitSTM` uses `registerFd` to register a callback for an ready event and returns a `STM ()` action that will block until the event occurs. Crucially it will block the green thread but the OS thread will return to the schedulers pool to run other Haskell code.

The scheduler is designed to wake up threads when the `STM ()` will no longer block. We can then use `threadWaitSTM` and related functions to write efficent non-blocking IO code. Here is an example inspired by the `network` package:

```haskell
len <- fix $ \next -> do
  lenOrError <- c_recvfrom fd ptr cnbytes flags ptr_sa ptr_len
    case lenOrError of
      -1 -> do
        err <- getErrno
        if err == eWOULDBLOCK || err == eAGAIN then
          threadWaitRead (fromIntegral fd) >> next
        else
          throwErrno loc
      x -> pure x
```

I don't think this code compiles. I expanded some function and simplified the actual function from `network` (and if it does it is not production ready look at [recvBuf](https://github.com/haskell/network/blob/master/Network/Socket/Buffer.hsc#L114) for a real example) but I think it gives the general idea.

You call `c_recvfrom` and if the result is `-1` and the `errno` value is `EWOULDBLOCK` you call `threadWaitRead` which blocks until the socket is ready. Once `threadWaitRead` returns you try again.

Yay! We get non-blocking IO under the hood but can program like we are making blocking IO calls.
