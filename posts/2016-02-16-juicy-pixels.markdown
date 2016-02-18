---
title: Artbitrary
author: Greg Hale
tags: CBaaS, haskell, development, unit-test
---

While setting up a test suite for [CBaaS](https://github.com/CBMM/CBaaS), I needed to produce random images. An `Arbitrary` instance for [JuicyPixel](http://hackage.haskell.org/package/JuicyPixels)'s `DynamicImage` type is the way to do this in Haskell. While producing a list of random `Pixel`s at different colors would have been enough to make the tests work, the output wouldn't be very interesting to look at - it would just be colorful white noise.

What if we use [QuickCheck](https://hackage.haskell.org/package/QuickCheck)'s ability to produce arbitrary functions, and pass an arbitrary `(Int, Int) -> Pixel` function into [`generateImage`](http://hackage.haskell.org/package/JuicyPixels-3.2.7/docs/Codec-Picture.html#v:generateImage)? Instead of fully random pixels, we would get random functions that determine a Pixel's color from its location (it may seem like the difference between random Pixels and random functions producing Pixels are one and the same - but the random pixels case is actually an extremely specific function).

How does QuickCheck produce random functions? What would I expect the output to look like? I have no idea, although I'd to look into it. In the meantime, here are some examples.

<aside style="background-color:#1f8dd6;width:80%;margin: 0 auto;color:white;margin:20px;padding:5px;border-radius:3px;font-size:10pt;">
NOTE: Some of the pictures appear smoothed. This isn't QuickCheck - it's just the browser scaling smaller images up to the same height as larger images.
</aside>

<div class="figure">
<img src="/images/arbitraryjuicy/4.png" style="height:200px;margin:2px;"/>
<p class="caption" style="border-style:solid;border-width:1px 0px 0px 0px;font-size:12px;">
An example image created with QuickCheck's Arbitrary function generator</p>
</div>

<!--more-->

Another generated image seems to have horizontal bands in one color channel. What sort of `(Int, Int) -> Pixel` would produce it?

![Different spatial frequencies in different color channels](/images/arbitraryjuicy/55.png)

<div class="figure"><div class="several"><img src="/images/arbitraryjuicy/5.png" class="juicy-example"/><img src="/images/arbitraryjuicy/37.png" class="juicy-example"/><img src="/images/arbitraryjuicy/61.png" class="juicy-example"/></div><p class="caption">An interesting common pattern: horizontal or vertical striations</p></div>



![A picture with a kind of even-odd structure in the rows](/images/arbitraryjuicy/97.png)


<div class="figure"><div class="several"><img src="/images/arbitraryjuicy/42.png" class="juicy-example"/><img src="/images/arbitraryjuicy/12.png" class="juicy-example"/><img src="/images/arbitraryjuicy/63.png" class="juicy-example"/></div><p class="caption">Not all of the pictures generated were this interesting. The majority looked like confetti-colored white noise, as you would expect from a function that throws away the input coordinates and returns a randomly colored pixel. Actually the last picture above is neat: half way through it changes spatial frequency.</p></div>

Some things decidedly *did not* show up in the output. I found no examples where the `x` and `y` axes were independent (no straight horizontal bars, straight vertical bars, or checkerboards). No simple dependencies showed up either (e.g. `if x == y then Blue else Green`). Many were not encodable in PNG and so aren't shown here. Many are solid black (QuickCheck focuses its testing effort on corner cases - I imagine `\(x,y) -> Black` is a value that gets picked often while other parameters (`width`, `height`) are varied. Below are all the pictures that came out in this test run, to give a sense of the distribution.

<style>

div.figure {
 margin: 40px;
}

div.figure div.several {
  display: flex;
  margin-left: auto;
  margin-right: auto;
  width: 80%;
}

div.figure div.several img{
  display: inline-block;
}

div.figure .caption{
  border-style: solid;
  border-width: 1px 0px 0px 0px;
  font-size: 12px;
}

div.figure img{
  display: block;
  height:100px;
  margin-left: auto;
  margin-right: auto;
}

.juicy-example {
  height: 100px;
  margin: 2px;
}
</style>

<img class="juicy-example" src="/images/arbitraryjuicy/0.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/10.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/12.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/13.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/14.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/15.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/16.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/18.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/19.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/20.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/21.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/22.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/25.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/27.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/28.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/29.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/3.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/30.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/31.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/34.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/36.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/37.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/38.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/39.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/4.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/40.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/41.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/42.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/44.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/46.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/47.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/48.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/49.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/5.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/51.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/52.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/53.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/55.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/56.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/6.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/60.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/61.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/62.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/63.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/64.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/65.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/67.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/68.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/7.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/70.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/71.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/72.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/75.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/78.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/79.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/8.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/80.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/81.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/82.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/84.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/85.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/86.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/9.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/90.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/91.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/92.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/93.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/95.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/96.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/97.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/98.png"/>
<img class="juicy-example" src="/images/arbitraryjuicy/99.png"/>

This is the code used to `Gen`erate an `Image a` for one of `JuicyPixel`s `Pixes` types. It [comes from](https://github.com/CBMM/CBaaS/blob/84c9607b9de856091344b13392022e5e1847af45/test/ModelSpec.hs#L77-L85) on [CBaaS](https://github.com/CBMM/CBaaS). There's nothing to it - all the magic is in QuickCheck's [`Function`](https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck-Function.html) module. `Proxy` isn't important to this post - it's used in my test suite to pick a particular `Pixel` type. 

~~~~ { .haskell }
genImg :: (Pixel a, Arbitrary a) => Proxy a -> Gen (Image a)
genImg _ = do
  wid <- choose (100, 300)
  hgt <- choose (100, 300)
  f :: Fun (Int,Int) a <- arbitrary
  let img = generateImage (curry (apply f)) wid hgt
  return img
~~~~
