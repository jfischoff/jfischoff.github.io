I've been training AnimdateDiff video models based on Stable Diffusion and something dawned on me. The loss function used for video generation is the same as the loss function used on image generation, but maybe this isn't optimal.

The simplest way to think of video is as a sequence of images. However, this is doesn't capture an important quality, mainly that each frame is related to the other frames.

## A Simple Example

To see why this is might be import let's consider a simple example. Imagine we have a three frame video with one pixel. To make things even simpler, each pixel value is 128.

```python
[128, 128, 128]
```

We train to video generation models. The first makes the following estimate for the frames after denoising (in Stable Diffusion we estimate the noise not the image, but let's ignore that detail for now).

```python
[129, 129, 129]
```

The second model makes the following estimate for the frames.

```python
[127, 129, 127]
```

The first estimate is more like the target. It is off by one, but the interframe differences are identical. The second model's inter-frame differences are off by two, e.g. it is flickering between the first and third frame. Although each frame is only off by one as well. Altough the first esimate is better, both models will acheive the same loss using the standard per frame reconstruction loss function.

The problem is that the standard reconstruction loss function looks at each frame independently. It is blind to interframe changes, but our brains are not. We are very good at detecting interframe changes and this is the key to what makes video interesting.

In this simple example the difference between the two models might seem small, but for a realistic example we would have 32x32 pixels or 1024 pixels, each free to flicker independently.

It is worth pointing out that the optimizing reconstruction loss does reduce these "difference" errors, but I wanted to see if we could improve the situation by directly optimizing to reduce them.

## A More Interesting Loss Function

A more accurate loss function needs to capture two characteristics of video, the path from one frame to the next and the appearance of individual frames.

The current loss function handles the latter, but what about the former?

The easiest thing to do is the compare the frame to frame diffs for the predicted and target frames.

Here is some code for computing the frame diffs of the target and predicted videos and then taking the mean squared error.

```python
def frame_diff(video_tensor):
    return video_tensor[:, :, 1:] - video_tensor[:, :, :-1]

def diff_loss(original_video, generated_video):
    # Calculate frame differences
    original_diff = frame_diff(original_video)
    generated_diff = frame_diff(generated_video)

    return F.mse_loss(original_diff, generated_diff, reduction='mean')
```

We also want to include the reconstruction loss. We care about the path through the space of images, but also the location. There is different ways we could do this. I utimately settled on an approach where I randomly choose a start frame to calculate the diffs from, instead of always being the first frame. I randomize it because I don't want the model to only learn the reconstruction of a specific frame. I then take the MSE of diffs and the start frame. So the actually diff loss code I use is more complex, here is a [link](https://gist.github.com/jfischoff/c7082d83963ae7bd745f6393e43a2d94).

## Is This Really Better?

It is not clear to me that this loss is actually better. For a loss function to be better, it is not enough for it to more accurately capture additional quality components we care about, it needs to work in practice.

Unfortuantely all versions of the `diff_loss` I tried have stability issues. The instability shows up primarily in the upblocks of the U-Net, specifically upblock 1. During training, the motion modules' attention matrices in upblock 1 have a tendency to loss their entropy quickly. This is another way of saying most of the entries in the attention matrix are close to one or zero, vs a smoother more uniform distribution. While this process occurs the gradients rise, and if the learning rate is too high, the model will diverge.

Just for the record, during training you want to see the attention maps entropy decrease, just in a stable way.

<table>
  <tr>
    <td>
      <img src="../images/diff_loss/high_entropy_attention_map.jpg" alt="High Entropy Attention Map" style="width: 250px;"/>
      <p align="center">High entropy attention map</p>
    </td>
    <td>
      <img src="../images/diff_loss/low_entropy_attention_map.jpg" alt="Low Entropy Attention Map" style="width: 250px;"/>
      <p align="center">Low entropy attention map</p>
    </td>
  </tr>
</table>

This same process occurs with the reconstruction loss if the learning rate is too high, it just happens with the `diff_loss` at a lower learning rate.

This process can be amerilirated during from-scratch training by using various transformer stability approaches like ["Query-Key Normalization"](https://arxiv.org/abs/2010.04245) or ["Sigma Reparam"](https://arxiv.org/abs/2303.06296). Also U-net stability [approaches](https://arxiv.org/abs/2310.13545), which target inherient instability in the upblocks of the U-net by scaling the skip connections also help. Helping fine-tuning is trickier because we can't change the model architecture easily.

My theory on why the loss is less stable, is that one, it creates a sharper loss landscape. This is consistent with attention entropy dropping during unstable training, but it is also common source of training instability, probably the most common. However, I think another reason it is unstable is that we are trying to predict diffs of the noise, not of the clean image. We want the network to learn that there is a smooth quality to the diffs, where the pixels of one frame are similar to the next. The noise doesn't have this quality, so I think it is harder to learn how to predict noise diffs, there is not really a pattern there.

My inituitive understanding of how the U-net works when predicting the noise, is that there is a noise-free semantic representation of the video in the deeper layers of the U-net. The conversion from this clean semantic representation to the actually noise prediction happens in the upblocks ... I think.

So, for fine-tuning with the `diff_loss` I experimented with training only the deeper blocks of the network, e.g. the down and mid blocks. This seemed to work better. When training the whole network, the reconstruction loss produces a noticable lower `diff_loss` indirectly by itself. However, when training just the deeper blocks, the `diff_loss` and reconstruction loss are comparably, and sometimes one does better than the other. I would say it is more of toss up.

## Results

Fine-tuning with the `diff_loss` produces interesting results. The animations tend to have more motion in general. Many end with a hand held video feel. This could be because it is better at predicting motion, or it could be an artifact from errors in the prediction getting turned into motion, or maybe something else. I'm not sure honestly, but it is interesting nonetheless.

Here are some examples of `diff_loss` on the left and reconstruction loss on the right.

<table style="width: 100%; border-collapse: collapse;">
  <tr>
    <td>
      <video style="max-width: 100%; height: auto; display: block;" controls src="../images/diff_loss/diff_loss_4.mp4" alt="" />
    </td>
    <td>
      <video style="max-width: 100%; height: auto; display: block;" controls src="../images/diff_loss/recon_loss_4.mp4" alt="" />
    </td>
  </tr>
   <tr>
    <td>
      <video style="max-width: 100%; height: auto; display: block;" controls src="../images/diff_loss/diff_loss_5.mp4" alt=""/>
    </td>
    <td>
      <video style="max-width: 100%; height: auto; display: block;" controls src="../images/diff_loss/recon_loss_5.mp4" alt=""/>
    </td>
  </tr>
  <tr>
    <td>
      <video style="max-width: 100%; height: auto; display: block;" controls src="../images/diff_loss/diff_loss_3.mp4" alt=""/>
    </td>
    <td>
      <video style="max-width: 100%; height: auto; display: block;" controls src="../images/diff_loss/recon_loss_3.mp4" alt=""/>
    </td>
  </tr>
  <tr>
    <td>
      <video style="max-width: 100%; height: auto; display: block;" controls src="../images/diff_loss/diff_loss_1.mp4" alt=""/>
    </td>
    <td>
      <video style="max-width: 100%; height: auto; display: block;" controls src="../images/diff_loss/recon_loss_1.mp4" alt=""/>
    </td>
  </tr>
  <tr>
    <td>
      <video style="max-width: 100%; height: auto; display: block;" controls src="../images/diff_loss/diff_loss_2.mp4" alt=""/>
    </td>
    <td>
      <video style="max-width: 100%; height: auto; display: block;" controls src="../images/diff_loss/recon_loss_2.mp4" alt=""/>
    </td>
  </tr>
</table>

You can see how the `diff_loss` has more camera motion. Overall I like most of the `diff_loss` generations slightly more, but they are pretty similar.

My hope was that I could perform a per pixel FFT of the `diff_loss` and the reconstruction loss and I would see a lower average value in the highest frequency component of the FFT, e.g. less flickering. However, when doing this [analysis](https://gist.github.com/jfischoff/35fc9220816029c53c3c37f9d07a702f) I found something different. The `diff_loss` had higher values in all of the frequency components except the first. This is just another way to say the animations move more. It is hard to tell if there is more less flickering, because small details are changing indirectly from the additional large scale motion. So the jury is out on if there is actually less flickering. If you know a better way to test for this, let me know!

## Conclusion

I think the idea of using some form of `diff_loss` is promising. Although the additional instability is a downside. However, it produces more dynamic results which is often what users want, so I think it is worth explorining further.

It is possible that using a larger batch size, like 1024, instead of the 16 I used, could improve the stability as well. Maybe when I'm not so GPU poor I could try that.

Something I would also like to try is using it with a base image model that predicts a clean image instead of noise. For now though I've pushed some checkpoints [here](https://huggingface.co/jfischoff/diff_loss_mid_down_blocks) for people to play with.
