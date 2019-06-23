---
layout: home
---

I'm Tao He. I'm a graduate student in Beihang University, majoring in
Computer Science and Engineering. I received my bachelor degree from
Beihang University at 2017.

./Computer Science and Engineering
----------------------------------

I'm a Haskell enthusiast. I have submitted some patches to GHC and hadrian.
I also contributed to many open source projects, such as pandas, mxnet,
pytorch, hindent.

./GSoC
------

I'm proudly a participant of **[GSoC 2019][5]** this summer in the Haskell community
supervised by [Ömer Sinan Ağacan][6], aimed at making the profiling tool
**[ThreadScope][7]** suitable for processing large eventlogs.

I write **[blogs](./topic/gsoc)** about the project regularly.

./Writings
----------

I write blogs regularly at Github Pages to record things inspire me along the
way of coding.

<ul>
  {% for post in site.posts limit:6 %}
    <li class="alink">
      <a href="{{ post.url }}" class="red-link">
        {{ post.date | date: "%Y-%m-%d" }}&emsp;{{ post.title }}
      </a>
    </li>
  {%- endfor -%}
  <li class="alink"><a href="./blog/" class="red-link">&hellip;&hellip;</a></li>
</ul>

./Gists
-------

+ [./.vimrc][1] I use VIM to write fancy programs.
+ [./cpp-tricks.md][2] Modern C++ is so amazing!
+ [./hemispheres.mma][3] Drawing is as easy as 1,2,3 with Mathematica.
+ [./&hellip;&hellip;][4]

./Pageviews
-----------

[1]: https://gist.github.com/sighingnow/086ac1b32f8ea3ba84d4
[2]: https://gist.github.com/sighingnow/505d3d5c82237741b4a18147b2f84811
[3]: https://gist.github.com/sighingnow/96946f539342085a0759474d5389af7a
[4]: https://gist.github.com/sighingnow
[5]: https://summerofcode.withgoogle.com
[6]: https://osa1.net/
[7]: https://wiki.haskell.org/ThreadScope
