
# The Bug Returns

`hasql-queue` was done but I wanted to release `hasql-notify` first. As I started to package up `hasql-notify` I realized every one of my tests used the same connection for sending and recieving notifications. This was the pattern that caused the bug. The pattern I dismissed as pointless.

Had I seen the bug? I had vague sense I might have seen `hasql-queue` tests block before.

Initially I was able to convince myself that I didn't need to worry about the bug, but it could make my tests flaky.

I began to worry more that I could not rely on PostgreSQL notifications at all until I understood why the bug was occuring.

I wanted to fix the bug.

As I began to think about how diagnose the issue I began more interested in fixing the bug. It was a different type of bug from what I typically fix. I wasn't immediately sure how to troubleshoot it but I felt like I might be able to use my "BPF Performance Tools" book.



The first thing I tried to address the bug was rewrite code. It was written in an unusual style that used `join` to combine results of `IO (IO a)`. I rewrote the code in a style I found easier to read.

It still had the bug.

I added logging to confirm I understood the execution path. It was working like I expected.

I switched from `threadWaitReadSTM`, which seemed unnecessary, to `threadWaitRead`.

After the various rewrites I was no closer to fixing the bug but I had introduced a new bug into the code. I'll get to that later.

## Theories

I formulated a few theories:

- There could be a bug in the test that, perhaps with the connection locking or with how it was using libpq directly that was the problem.
- The implementation could have bug. Possibly the `epoll_ctl` was not called at the time.
- I wasn't sure on

## Understanding epoll_wait

`epoll` events do not work the way I assumed. I assume that when a socket was ready it alert the epoll interest table adding an itself to some recent event table.

AFAICT (and I really don't have a great grasp) it is some sort of two step process. There is some step like the one I mention above, but the socket doesn't update the an event table. It is like a list of files for epoll to call `poll` on and see what events occured.



## Digging Deeper

At this point I had convinced myself the Haskell code was not the problem. It was something else.

I began to read the code.

The `epoll_wait` documentation is misleading:

```
A call to epoll_wait() will block until either:
  · a file descriptor delivers an event;
  · the call is interrupted by a signal handler; or
  · the timeout expires.
```

The first bullet should really say:

```
 · a file descriptor delivers an event and the event is not handled during the course of epoll_wait trying to retrieve it;
```

I mean, I see why they did not write this but this is the root of all the `threadWaitRead` confusion. `epoll_wait` is designed to minimize unnecessary wake ups and will decide to ignore a wake up if file descriptor is no longer ready.

When things workout, the `epoll_wait` thread wakes up and calls `tcp_poll` which confirms there is data to read on the socket. It will then return.

Here is the sequence of events.

![Good epoll_wait](./hardest-wont-fix/good_epoll_wait.png)

When things fail, `recvfrom` clears the data off the recieve queue on the socket before the `epoll_wait` thread can call `tcp_poll`. By the time it does call `tcp_poll` it gets a `260` which is equivalent to a `s` event mask, instead of a `325` which is equivalent to a `s` event mask.

This causes the `epoll_wait` thread to go back to sleep and `threadWaitReadSTM`'s action never returns.

![Bad epoll_wait](./hardest-wont-fix/bad_epoll_wait.png)

# What I Should Have Done

The path I took to understand the bug was circustus. In retrospect there was a quicker way to understand the bug.

I began by adding logging into the GHC RTS. This was unnecessary. All of the information I was able to get from adding logging I could have also got from `bpftrace`.

When I began using `bpftrace` I wanted to following the path of packets through the network stack and see where they got stuck. This was a

Some mistakes I made in no particular order:
- I assumed I understood the semantics of `epoll_wait`. Additionally I assumed it was documented in a clear way. I think the take away is Linux system calls can have subtle semantics that are not necessarily documented. I'm not sure what the best way to learn the semantics is, but tracing the execution of `do_epoll_wait` and related functions is how I was able to determine the semantics this time.

The key breakthrough was learning the semantics of `epoll_wait` through tracing.

I started by wanting to trace the callback that signified data was ready to read on the socket. This was the right idea but I intitially drew the wrong conclusion because I misintrepeted the data.

The problem was I was printing out info to stdout from Haskell and printing using `bpftrace` and the timing of the `bpftrace` output was delayed compared to the Haskell output. So I thought there was not a `socket_def_ready` call for the new data but there was. I just didn't see it.

I think the take away is that I need a better way to visualize the execution path of functions I was tracing. Just looking at the printed out functions is easy to misread. I need a way to make something akin to `jaeger` output.

Once I knew that the socket was calling the ready callback I was confused why `epoll_wait` was not returning. I verified the arguments passed to `epoll_ctl` were correct. I already suspected they were because I had traced out the GHC code, but this something I could have done first and not bothered with tracing GHC at all.

I wanted to know where `epoll_wait` was blocked. So I started to trace out all the related functions. This is how I learned the `epoll_wait` thread was waking up.

The process I was taking was a mixture of reading the Linux source code, followed by tracing out the functions I would read about. Essentially I was trying to understand how the code worked. In retrospect this was the right idea.

I'm trying to think how I could have done this earlier. There were so many pieces I identified that were involved with this bug that I did not understand. If this happens again where should I start?

After confirming the Haskell code was correct, I feel like a binary search through the stack was probably the best idea. If the event manager was the top and linux internals the bottom the middle was probably the GHC scheduler. So I could have started by verifying if the thread wakes up or not.

I did do this using the eventlog. The thread was blocked in FFI forever. So `epoll_wait` was not returning. At this point I could have traced out all the child functions of `epoll_wait`. I then would have seen it woke up.

I did conclude this and realized I need to know how `epoll_wait` works. I theorized that `epoll_wait` was waking up but something was clearing events before it could process them.

The only thing I knew that could do this was `close `. So I traced out all `close` calls. Didn't see anything. I then decided to trace out all syscalls. This is when I noticed the `recvfrom` before epoll related functions.

I looked the code of `recvfrom` and could not see how it was affecting `epoll_wait`. It was only because I happened to read a blog post that briefly mentioned the use of `tcp_poll` to verify socket was still available for writing that I was able to add the key trace to convince myself that my theory was correct.

Debugging the Linux kernel code would have helped learn how `epoll_wait` worked and would have helped me find the `tcp_poll` on my own. I feel like this should be something in my toolbox. Something to try next time I need to understand the behavior of complicated linux call in detail.

Tracing is good but you need to know what you are looking for. Because the Linux kernel utilizes functions pointers it is not easy to look some of the code and statically know what the callgraph is going to be. This is where debugging could come in ... I assume. I am not sure If I will need to more than configuring the kernel to have debug symbols and stepping into system calls ... I guess I'll find out next time!



When I started looking at this bug I didn't know any of this. I assumed the `hasql-notification` bug was something simple. Then I decided to ignore it because I wan't to see if `hasql-queue` was faster than `postgresql-simple-queue`. It was and it was only when I was ready to release



GHC's runtime has a very nice IO manager. The IO manager, or more precisely the `EventManager`, allows Haskellers to program as though they are making blocking IO calls but under the hood the `EventManager` works with the OS's multiplexing IO APIs (`kqueue` and `epoll`) to implement non-blocking IO. This is huge win because blocking IO is a simpler model to program against but Haskellers still get the performance benefits of non-blocking IO.

## A Quick Tour of the `EventManager`

The `EventManager` has a very simple API. I want to focus on the most important function:


```haskell
registerFd :: EventManager -> IOCallback -> Fd -> Event -> Lifetime -> IO FdKey
```

This function lets one express interest in file descriptor ready events. For one can register a callback that will get called when a file descriptor is ready for reading:

```haskell
registerRead fd f = do
  Just ev <- getSystemEventManager
  let callback fdKey event = when (event == evtRead) $ f fdKey
  registerFd ev callback fd evtRead OneShot
```

The function above lets on register a callback that is called when the file descriptor is ready for reading. In other words we will print out "Read Callback ..." when there is data to read on file descriptor. You can ignore the `OneShot` part. The event manager only supports `OneShot` callback semantics.

This is a neat trick but is critically important when working with non-blocking file descriptors. Consider the POSIX function `recvfrom` which is to used to recieve bytes on a socket. In typical blocking IO usage, a call to `recvfrom` on an empty socket with block the thread until data arrives in the socket recieve buffer. This not a problem by itself. However if one naively tried to make a server which used a separate thread for every connection it would be an inefficent use of OS threads.

Non-blocking IO is useful because it faciliates a pattern of servicing a large number of sockets using a much smaller pool of threads. When one calls `recvfrom` on a non-blocking empty socket it returns `-1` and sets the `errno` to `EWOULDBLOCK`.

The OS is essentially telling us the socket is empty and we should call `recvfrom` later. We have a few options. We can poll the socket but doing so is inefficent or introduces unnecessary latency or both. A much better option is to register a callback for to call when the socket is ready for ready.

Registering interest in ready events is one piece of implementing non-blocking IO efficently but we still need a way to schedule the work on our pool of worker threads.

Luckily for the `EventManager` implementators GHC already had a preemptive scheduler utilizing light weight green threads. The scheduler takes care of running Haskell code on the OS threads.

To take advantage of the GHC green threads we need a function like `threadWaitSTM`

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

We can use `threadWaitSTM` and related functions to write efficent non-blocking IO code. Here is example inspired by the `network` package:

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

I don't think this code compiles (and if it does it is not production ready look at https://github.com/haskell/network/blob/master/Network/Socket/Buffer.hsc#L114 for the details) but I think it gives the general idea.

You call `c_recvfrom` and if the result is negative one and the `errno` value is `EWOULDBLOCK` you call `threadWaitRead` which blocks until the socket is ready. Once `threadWaitRead` returns you try again.

Yay! We get non-blocking IO under the hood but can program like we are making blocking IO calls.

This is the main use case for `threadWaitSTM` but clever Haskellers have found other uses for the ready events. For instance this code in `postgresql-simple` use `threadWaitReadSTM` to efficently wait for

# TODO talk about MVars and the GHC scheduler.

 MacOS and Linux and in general hides the implementation details so typically Haskellers don't have to worry about the details of non-blocking IO but still get the performance benefits.

Haskellers will are able to write programs as though they are using blocking IO calls,



# The Hardest Won't Fix Bug I Worked on for "Fun"

"The path is goal", I keep telling myself. It is hard to shake your childhood religion.

The hardest bug I remember working on is done. I'm relieved. It's done. I can finish releasing `hasql-queue`.

The bug is not fixed. It's still there. Read's the docs of `libpq-notify` and you'll see.

Do I wish I could just fix it? Eradicate it from existance. I would be lying if I didn't admit that "Won't Fix" doesn't have the same ring as "Done". Complete success vs deciding it's not worth it. After two months I have succeeded in convincing myself the bug doesn't matter. The workaround I came up with when I knew nothing is same one I have now. I am exactly in the same place I started. I just know why I shouldn't care. If I could do it all over again I would.

I knew if I figure out the bug I would have to learn something new. Something interesting.

It was an increasingly difficult challenge.

I kept working on the bug because I liked what I was learning. I can't do this at work so it was fun to have no time constraints.

I was going to figure out this bug or die trying.


# OLD STUFF

In January I was readying a new version of `postgresql-simple-queue`. I was able to run some full system callgraph profiling on it and it was clear that the old style text based PostgreSQL client protocol `postgresql-simple` uses was slowing down the throughput. I needed to use the newer binary protocol like what is used in `hasql` or `squeal`.

`squeal` is very cool. We use it at work a lot. `mwotton` and `eitan` have done some wonderfully things with `squeal`. They've written these expressive, performant, composable, huge and maintable queries with it. One day I will try it, but today (January) was not the day. Also I think fancy typed things are annoying ... usually.

I started looking at `hasql` and realized that there was no `NOTIFY` (PostgreSQL notification) support.

Then I came across `cocreature`s `https://github.com/cocreature/hasql-notifications` and found this bug:

TODO INSERT TWITTER


I believed I could fix this bug. I didn't understand the discussion very well.

> Me: Sure they had tried to reason through the bug but did they try Wireshark?

As is on brand for me, I assumed I would fire up Wireshark and crush this bug.

I forked the project and made it a little easier for me to run the test. I was able to repro the test.
