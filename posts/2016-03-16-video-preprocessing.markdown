---
title: Processing video for web experiments
author: Greg Hale
tags: video, open science
---

So you want to put video clips online for your experiment. In this article I'll introduce you to some tools for processing video files for use in experiments. In another I'll describe acquiring data from subjects watching videos.

I'm here to help with this kind of thing. Please get in touch if needed! If you want to learn to do it for yourself, that's also fantastic, and I hope this guide helps. If you discover a problem with the instructions, please let me know on our [issue tracker](https://github.com/CBMM/CBMM.github.io/issues).

Where to start? There are three phases to think about:

 1. Creation
 2. Storage & Addressing
 3. Delivery

<!--more-->

And here are some of our design considerations:

 - Formats should be playable in most web browsers
 - Files should be small enough for quick loading
 - Files should be large enough to avoid visible artifacts
 - Naming schemes shouldn't lead to subtle bugs
 - Naming schemes prevent subjects or random people from downloading and watching the clips ahead of time

Let's take these one at a time, starting from a common user story: having one long video that we want to chop into smaller parts.

The easiest way to embed video into a page is with the HTML5 `<video>` tag, like so:

```html
<video width="640" height="480" controls>
  <source src="clip-000001.mp4"  type="video/mp4"/>
  <source src="clip-000001.ogg"  type="video/ogg"/>
  <source src="clip-000001.webm" type="video/webm"/>
  <div class="error">
    Video is unsupported for your browser,
    please get in touch with mygradstudent@mit.edu
  </div>
</video>
```

The `<video>` tag is a bit like `<ul>` as it contains several elements inside: one `<source/>` listing the path of the video file... for each format you want to supply. Different browsers support different formats; mp4 is widely supported, with ogg and webm serving as fallbacks[^vidcompat].

[^vidcompat]: Mozilla Developer Network [documentation](https://developer.mozilla.org/en-US/docs/Web/HTML/Supported_media_formats) about browser media support. [MDN](https://developer.mozilla.org) is a really wonderful resource!

## Transcoding

The first thing to do is to get your source video into this format. [`ffmpeg`](https://www.ffmpeg.org/) is the correct tool for this. If you are on OSX, we recommend installing software via (the [homebrew](http://brew.sh)):

`brew install ffmpeg`

If you are on Ubuntu:

`sudo apt-get install ffmpeg`

`ffmpeg` is a real swiss-army knife. Learning to use it well is an investment with a large payoff - you can do a lot with video files that isn't possible is, say, matlab. Read the [documentation](https://www.ffmpeg.org/ffmpeg.html) before you try to do anything other than the specific tasks in this article. For now, just run:

```
ffmpeg -i path/to/video.m4v -strict -2 -b:v 2M video.mp4
ffmpeg -i path/to/video.m4v -strict -2 -b:v 2M video.webm
ffmpeg -i path/to/video.m4v -strict -2 -b:v 2M video.ogg

```

There, we assume you have a `m4v` source video (this format isn't supported in web browsers) at `path/to/video.m4v`. The `-i` flag indicates that the following argument is an input. `-strict -2` is a flag to turn on `m4v` decoding (which is currently 'experimental', so it's not available by default). `-b:v 2M` tells `ffmpeg` to use a 2MB/sec encoding rate for the output video. If you don't like the size or quality of the output, tweak this parameter. The final argument is the output file in the format you want (`ffmpeg` picks the right encoding based on the file extension you use here). Depending on the format, expect transcoding to run between 0.2x and 5x of real time - a 1 minute video will take anywhere from 5 minutes to 12 seconds.

## Unfolding

The next thing we often do is extract lots of clips from the video[^singleclip]. For all the things `ffmpeg` can do, unfolding a long video into a long list of short videos efficiently is not one of them. I wrote a command line utility called [CutAtFrames](https://github.com/CBMM/videoutils/releases)([repo](https://github.com/CBMM/videoutils)) for this. `CutAtFrames` takes a path to an input video, a path to a file with a list of cut frames, the framerate, and an output directry. It first breaks the input file into ~2 minute chunks[^chunks], and then divides those chunks into clips at the boundaries that you specify in the cut frames file. Seeking and copying from the video file is not very fast, so `CutAtFrames` generally takes a few hours to run for a two hour movie cut into ten second clips.

[^chunks]: Why not divide the big video directly into the desired clips? It turns out that this is too slow. Seeking to the first frame of a clip take time proportional to the number of frames before the target. Assuming that your cuts are near some constant size, the number of cuts is proportional to the length of the video, so the total run time is proportional to the numer of cuts squared. Breaking the file into large chunks, we still have an $O(n^2)$ problem, but the  constant factor is far smaller. Without pre-chunking, breaking a feature film into 5-second chunks would take about a week. With 2-minute pre-chunking it can be done in a few hours.

The contents of the cut file is a single integer marking the last frame of the clip (the first frame is inferred to be the one after the last frame of the last clip - the next version may take frame ranges here instead). A sample run looks like this:

```bash
./CutAtFrames -i Videos/pointlights.mp4 -c Videos/pcuts.txt -f 29.97 -b 120 -o ./mp4/
```

[^singleclip]: Alternatively maybe we could embed a single large video file, and to divide it into trials, just programatically seek to different parts of the video. That is probably a very nice solution to the problem of breaking a video into small trials, but I haven't tried it yet to let you know how it goes. At the least, we will have to write a little javascript to seek to parts of the video, stop at the correct interval, and buffer.

