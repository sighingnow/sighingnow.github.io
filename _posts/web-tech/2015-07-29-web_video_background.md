---
title: 将视频作为网页背景
author: Tao He
date: 2015-07-29
tags: [Web-Front-End]
category: Web-Tech
layout: post
---

一些门户网站采用视频作为网页背景，具有很好的展示效果，而随着HTML5技术的发展和浏览器支持程序的提升，采用视频作为网页背景也变得越来越容易。

技术分析
-------

CSS里的`background-image`属性只能使用图片、SVG、颜色或渐变色。但从技术讲，我们是可以伪造出一种效果，让视频以背景的角色出现在其它HTML元素后面。这其中的难点是视频要填充整个浏览器页面，而且要响应浏览器窗口大小的变化。

视频作为网页背景，需要考虑一下这几个问题：

1. 作为背景的视频应该设置为自动播放(set to `autoplay`)，而默认状态下应该是关闭声音(视频里面最好不含声音)。
2. 背景视频应该有个替代图片，当浏览器不支持这种HTML5技术、视频格式时用图片替代。
3. 视频长度很重要(12-30秒之间)。
4. 视频的体积应很小，尽量的压缩。同时，它需要在不同尺寸设备上自动的适应屏幕大小。如果有可能，应该使用JavaScript控制对不同的屏幕大小加载不同分辨率的背景视频。

<!--more-->

视频格式
-------

当前，video 元素支持三种视频格式：

| Format  |  IE  |  Firefox  | Opera   |   Chrome  |  Safari |
|---------|------|-----------|---------|-----------|---------|
| Ogg     | No   | 3.5+      | 10.5+   | 5.0+      | No      |
| MPEG4   | 9.0+ | No        | No      | 5.0+      | 3.0+    |
| WebM    | No   | 4.0+      | 10.6+   | 6.0+      | No      |

视频格式说明：

+ Ogg = 带有 Theora 视频编码和 Vorbis 音频编码的 Ogg 文件
+ MPEG4 = 带有 H.264 视频编码和 AAC 音频编码的 MPEG 4 文件
+ WebM = 带有 VP8 视频编码和 Vorbis 音频编码的 WebM 文件

具体实现
-------

HTML5支持`video`标签，因此，很容易插入一段视频：

~~~html
<video autoplay loop poster="polina.jpg" id="bgvid">
  <source src="polina.webm" type="video/webm">
  <source src="polina.mp4" type="video/mp4">
</video>
~~~

这里摆放视频格式的顺序很重要，因为有些版本的谷歌浏览器里，如果`.webm`格式的视频放在了其他视频后面，视频将无法播放。

**我们使用视频的第一帧图像作为视频的封面图片**，这样，当背景视频一旦加载完成，我们可以看到很流畅的从图片过度到背景视频播放。

接下俩，通过CSS控制视频全屏：

~~~css
video#bgvideo {
    position: fixed; right: 0; bottom: 0;
    min-width: 100%; min-height: 100%;
    width: auto; height: auto; z-index: -100;
    background: url(polina.jpg) no-repeat;
    background-size: cover;
}
~~~

注意需要在CSS中使用`background`属性来控制页面背景，在较老版本的浏览器上，当视频无法播放时，使用背景图片作为网页背景。

HTML 5中的`video`标签支持通过`control`属性供添加播放、暂停和音量控件。例如：

~~~html
<video controls="controls" width="800" height="480">
    Your browser does not support the video tag.
    <source src="polina.webm" type="video/webm">
    <source src="polina.mp4" type="video/mp4">
</video>
~~~

<video controls="controls" width="800" height="480">
    Your browser does not support the video tag.
    <source src="{{site.url}}/resource/web_video_background/polina.webm" type="video/webm">
    <source src="{{site.url}}/resource/web_video_background/polina.mp4" type="video/mp4">
</video>

在网页上显示内容
--------------

要想在以视频为背景的网页上显示内容，只需要创建一个`div`做容器，在设置CSS样式即可：

~~~html
<div id="polina">
<h1>POLINA</h1>
<p>filmed by Alexander Wagner 2011</p>
</div>
~~~

控制样式：

~~~css
#polina {
    font-family: Agenda-Light, Agenda Light, Agenda, Arial Narrow, sans-serif;
    font-weight: 100;
    background: rgba(0,0,0,0.3);
    color: white;
    padding: 2rem;
    width: 33%;
    margin: 2rem;
    float: right;
    font-size: 1.2rem;
}
~~~

控制视频播放
----------

可以通过创建一个`button`来控制背景视频的播放，只需要同构javascript脚本来绑定事件即可：

~~~html
<button>Pause</button>
~~~

CSS：

~~~css
#polina button {
    display: block; width: 80%; padding: .4rem; border: none; margin:1rem auto;
    font-size: 1.3rem;
    background: rgba(255,255,255,.23);
    color: #fff;
    border-radius: 3px;
    cursor: pointer;
    -webkit-transition:.3s background;
    transition: .3s background;
}

#polina button:hover{
    background: rgba(0,0,0,.5)
}
~~~

事件脚本：

首先，通过`document.querySelector`获取`button`控件，然后通过`addEventListener`绑定事件即可。

~~~javascript
var video = document.getElementById("bgvideo"),
    pauseButton = document.querySelector("#polina button");
pauseButton.addEventListener("click",function() {
    video.classList.toggle("stopfade");
    if(video.paused) {
        video.play();
        pauseButton.innerHTML="Pause";
    }
    else {
        video.pause();
        pauseButton.innerHTML="Paused";
    }
},false);
video.addEventListener('touchstart',function(e){e.preventDefault();video.play();})
~~~

在线演示：[将视频作为网页背景(演示)]({{site.url}}/resource/web_video_background/web_video_background.html){:target="_blank"}。

参考
----

1. [Create-Fullscreen-HTML5-Page-Background-Video](http://demosthenes.info/blog/777/Create-Fullscreen-HTML5-Page-Background-Video)

