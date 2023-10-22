---
title: Pipeline Model Parallelism in LLM Pretraining
author: Tao He
date: 2023-09-16
tag: [LLM]
category: Machine Learning
layout: post
---

Pipeline model parallelism is a technique which is aimed to make training
extremely large neural network models possible by splitting the model in
the layer dimension and running each stage on a different device. This is
the key technique used to train the state-of-the-art large language models (LLMs).

<!--more-->

GPipe: F-then-B
---------------

[GPipe][1] introduces the F-then-B (forward-then-backward) pipeline parallelism
with micro-batching. The idea is to split the model into $N$ stages, and
run each stage on a different device. For a given mini-batch, GPipe first evaluates
the forword computation, then backpropagates the gradients through the model.
To reduce the bubbles in the pipeline, GPipe uses micro-batching, which splits
the mini-batch into $N$ micro-batches, and each micro-batch can be processed at
a different stage. The gradients are accumulated across micro-batches and then
used to update the model parameters. The micro-batch optimization can be thought
as some kind of optimization like loop unrolling. In GPipe, the performance increases
when the micro-batch size increases, while keeping the exactly same graident update
semantic with the vanilla gradient descent optimization.

![F-then-B scheduling]({{site.url}}/resource/pipeline_parallelism/f-then-b.png)

PipeDream: 1F1B
---------------

[PipeDream][2] introduces another pipeline parallelism scheduling scheme called
1F1B (1 forward and 1 backward). Like GPipe, PipeDreams splits the model to stages
in the layer dimension and splits a mini-batch to micro-batches as well. Unlike
GPipe which first evaluates the forward computation for all micro-batches before
the backward computation, PipeDreams interleaves the forward and backward computation
by running the forward and backward computation one by one for each micro-batch.
To maintain the same graident update semantics with the vanilla gradient descent
optimization, PipeDream requires a warm-up period to filling batches from an empty
pipeline state and a tear-down period to wait the backward computation for all
micro-batches finish.

![1F1B scheduling]({{site.url}}/resource/pipeline_parallelism/1f1b.png)

There are still bubbles, but the 1F1B scheduling reduces the memory usage to
save activations (GPipe mitigates the issue by recomputation) for batches whose
backward computation hasn't finished yet.

PipeDream-2BW: Asynchronous 1F1B
--------------------------------

The same team of PipeDream develops [PipeDream-2BW][3] to elimnates the bubbles
between each mini-batches, by introduce a exact 1-step delayed asynchronous gradient
update. PipeDream-2BW introduces a copy for the optimizer state, and allow the
gradient update be applied to the newer version of the model parameters, avoiding
the graident flush and waiting wall between mini-batches.

![Async 1F1B scheduling]({{site.url}}/resource/pipeline_parallelism/async-1f1b.png)

Asynchronous 1F1B looks quite promising as it eliminates all the bubbles in the
the pipeline. When the pipeline parallelism goes deeper for larger language models,
the performance gain can be significant. However, the popular LLM training frameworks
(e.g., Megatron-LM, DeepSpeed) don't adopt such a design. Why? One possible reason
is the training stability. Althrough the PipeDream-2BW paper shows it works for
GPT-like models and there are many research works on asynchronous training, the
convergence of asynchronous training is not guaranteed and still an open question.
Consider the cost of training large language models, with asynchronous training,
there are risks to get a bad model, while without the optimization, only less than
$20%$ performance gain is lost. Consider the actual utilization of GPU clusters
for LLMs is fairly low, the risk might be not worth taking.

Eager 1F1B
----------

The 1F1B scheduling scheme was optimized by [Eager 1F1B][4] in another direction:
improve overlapping of communication and computation. From the 1F1B scheduling
we could find out that the forward computation in the $i$-th stage is happening
immediately after the $i-1$-th stage, making it hard to overlapping this cross-stage
communication with other computation works. Eager 1F1B introduces a modification
on 1F1B to compute one more micro-batch in the prevous stage to overlap the
communication with the backward computation happening in the next stage, at the
cost of larger memory usage to store the activations for the additional micro-batch.

![Eager 1F1B scheduling]({{site.url}}/resource/pipeline_parallelism/eager-1f1b.png)

The Eager 1F1B can be combined with the asynchronous 1F1B as well:

![Eager 1F1B scheduling]({{site.url}}/resource/pipeline_parallelism/async-eager-1f1b.png)

[1]: https://arxiv.org/abs/1811.06965
[2]: https://arxiv.org/abs/1806.03377
[3]: https://arxiv.org/abs/2006.09503
[4]: https://arxiv.org/abs/2211.05322
