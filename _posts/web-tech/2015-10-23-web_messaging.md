---
title: Web Messaging 以及 jschannel.js 用于页面间跨域通信
author: Tao He
date: 2015-10-23
tags: [Web-Front-End]
category: Web-Tech
layout: post
---

Web Messaging (Cross-document Messaging) 技术是 HTML5 规范中定义的允许不同源站点的页面进行通信的应用程序接口。可以有效地解决在
同源策略的限制下不同站点的页面不能有数据交互的问题。

<!--more-->

同源策略
-------

同源策略(Same-origin Policy)，是Web应用程序安全模型中的重要概念，在同源策略的限制下，仅当两个文档页面(html)同源时，两个页面间也允许读取对方页面上的数据。同源(same orgin)的具体含义：

+ same url scheme
+ same hostname
+ same port

当我们谈论如何使用JavaScript访问DOM时，我们便需要考虑同源策略的这三个要素：URL协议、主机名、端口号，当我们使用浏览器同时打开两个不同站点的页面时，在同源策略的保证下，一个页面无法直接读取另外一个页面中的数据。

来自于同一个根域，但不同子域的页面之间也无法直接进行数据交互，例如，a.xxx.com 与 b.xxx.com 的页面之间就无法直接使用js来读取对
方dom节点中的数据。如果要允许这两个页面进行交互，需要先将这两个页面的 `document.domain` 属性设置为根域(xxx.com)。

    document.domain = 'xxx.com'

这表示，在当前页面， `xxx.com` 下的任何站点都可以访问当前页面的 DOM 资源。

使用XMLHttpRequest对象发起HTTP请求(Ajax)就必须遵守同源策略。具体而言，Web应用程序能且只能使用XMLHttpRequest对象向其加载的源域名发
起HTTP请求，而不能向任何其它域名发起请求。跨源资源共享这种机制让Web应用服务器能支持跨站访问控制，从而使得安全地进行跨站数据传输成为可能。要想
使子域与根域之间有一个双向的 XMLHttpRequest 通信通道，服务器需要访问如下的响应头：

    Access-Control-Allow-Origin: *.Httpsecure.org
    Access-Control-Allow-Methods: OPTIONS, GET, POST, HEAD, PUT
    Access-Control-Allow-Headers: X-custom
    Access-Control-Allow-Credentials: true

对于Ajax的跨域问题，可以使用JSONP，或者在页面中嵌入IFrame将XMLHttpRequest转换成提交表单来解决。

Web Messaging
--------------

同源策略引起的不同源页面的数据交互的限制，可以使用 Web Messaging API 来解决。具体来说，使用`window`对象的`postMessage`方法向另一
个`window`发送纯文本的内容，然后，另一个`window`可以监听到这个消息，然后调用注册到`message`事件的回掉函数来处理消息中的数据。举例：

父级页面：

域：`http://www.a.com`

~~~html
<div>
    <input type="text" id="message" />
    <button onclick="sendMessage();">Send Message</button>
    <iframe id="remotepage" src="http://www.a.com/addr/target.html"></iframe>
</div>
<script type="text/javascript">
    function sendMessage() = {
        $('#remotepage').contentWindow.postMessage("plain text message", "http://www.example.com");
    }
</script>
~~~

子页面：

域: `http://www.b.com`

~~~html
<div id="messagebox"></div>
<script type="text/javascript">
    function receiver(event) {
        if(event.origin == "http://www.a.com") {
            alert("get message: " + event.data + " from " + event.origin + ", source window: " + event.source);
            // event.source.postMessage("got it!");
        }
    }
    window.addEventListener('message', receiver, false);
</script>
~~~

子页面中的最后一条语句`window.addEventListener('message', receiver, false);`用来绑定事件，当`window`接收到一个message时，就会调用这个函数进行处理。如果使用 jQuery 来绑定事件：

~~~javascript
window.on('message', function (event) {
    alert(event.originalEvent.data);
});
~~~

需要注意的是，jQuery对于message事件做了一层封装，要想获取到 `event.origin`, `event.data` 等属性，需要使用`event.originalEvent`。

jschannel.js
------------

[jschannel.js](https://github.com/mozilla/jschannel) 是 Mozilla 开发的一个页面间使用 Web Messaging 技术进行通信的JS库。

> A JavaScript library which implements fancy IPC semantics on top of postMessage.

jschannel.js 将页面间的方法调用封装成 json 文本，使用 postMessage来发送消息，在另一个页面上，按照同样的协议来解析消息内容，调用对应的函数并最终返回函数的执行结果给另一个页面。

+ Page Setup

making a simple HTML page that includes the JSChannel script:

    <script src="jschannel.js"></script>

And a trivial child frame:

    <iframe id="childId" src="child.html"></iframe>

+ Creating a Channel

Let's create a channel in our child frame:

~~~javascript
chan = Channel.build({
    window: document.getElementById("childId").contentWindow,
    origin: "*",
    scope: "testScope",
    onReady: function() {
      console.log("channel is ready!");
    }
});
~~~

+ Remote Methods

Now we'll define a simple function in our child frame:

~~~javascript
chan.bind("reverse", function(trans, s) {
    return s.split("").reverse().join("");
});
~~~

And call it in our parent frame.

~~~javascript
chan.call({
    method: "reverse",
    params: "hello world!",
    success: function(v) { emit(v.toString()); }
});
~~~

更详细的 jschannel.js 使用的方式，可以查看[文档](http://mozilla.github.io/jschannel/docs)。

