A trick I discovered when hacking on AnimateDiff motion modules: you can increase and decrease the amount of motion by adjusting the temporal attention "scale."

If you remember the attention formula, before taking the softmax we multiply by `1/sqrt(d)`. This often called the "scale".
If we adjust the numerator to be greater than 1, we get more motion and if we adjust it to be less than 1 we get less motion.

It works okay as way scale the motion, although if you make it too high you will get artifacts and if you make it too low you will no motion at all.

I wanted to see how this attention scaling affected Stable Video Diffusion (SVD).

It does. It affects the motion in a similar way to the `motion_bucket_id`, but with somewhat difference results.

scale 0.5, motion_bucket_id 512, fps_id 12, 14 frames 4x interpolated

![Scale 0.5](../images/motion_control_with_attention_scaling/scale_0.5.mp4)

scale 1.0, motion_bucket_id 512, fps_id 12, 14 frames 4x interpolated

![Scale 1.0](../images/motion_control_with_attention_scaling/scale_1.mp4)

scale 2.0, motion_bucket_id 512, fps_id 12, 14 frames 4x interpolated

![Scale 2.0](../images/motion_control_with_attention_scaling/scale_2.mp4)

Is this better worse than the `motion_bucket_id` way? I don't know. I think it is just another knob that is nice to have so I thought I would mention it to a wider audience.

I haven't added a great way to control the attention scale in `sgm`. I'm waiting on the diffusers code to drop, but for now you can try a very hacky change I made to `sgm` [here](https://github.com/Stability-AI/generative-models/compare/main...jfischoff:generative-models:main#diff-cfa95be7dddd86c8097a6548aa7bb2cd63ce551b4e28d1f21fd33380eb812548R417).

Basically I detect it is temporal attention if the sequence length is the number of frames. If so I use a different scale.

When `diffusers` gets a real `SVD` implementation I will add a better way to control the attention scale.

The paper I got this idea from is ["Training-free Diffusion Model Adaptation for
Variable-Sized Text-to-Image Synthesis"](https://arxiv.org/abs/2306.08645) which used a similar trick to allow Stable Diffusion to generate images smaller than what it was trained on.
