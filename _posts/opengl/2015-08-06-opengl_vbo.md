---
title: OpenGL顶点缓冲对象(VBO)
author: Tao He
date: 2015-08-06
tag: OpenGL
category: Graphics
layout: post
---

OpenGL是按照客户机-服务器模式设计的，在OpenGL需要数据的任何时候，都必须把数据从客户机内存传输到服务器。如果数据并没有修改，或者客户机和服务器位于不同的计算机（分布式渲染），数据的传输可能会比较缓慢，或者是冗余的。当我们使用顶点数组时，可以把单个数组从客户内存（CPU可以访问）传输到图形硬件。OpenGL2.1开始，增加了顶点缓冲区对象（Vertex Buffer Object）的特性，这个特性允许我们按照类似于管理纹理的方式来管理顶点数组数据，相比于glBegin/glEnd这种方式，顶点缓冲对象更加高效，而且顶点缓冲区对象要更灵活。

<!--more-->

创建顶点缓冲对象
-------------

VBO依赖glew库，因此，在使用VBO之前，需要初始化glew：

~~~c
GLenum res = glewInit();
if(res != GLEW_OK) {
    fprintf(stdout, "Error: '%s'\n", glewGetErrorString(res));
    return EXIT_FAILURE;
}
~~~

>  GLEW helps you deal with the headache that can accompany the management of extensions in OpenGL. Once initialized it queries for all the available extensions on your platform, dynamically loads them and provides easy access via a single header file.

创建了一个顶点数组：

    float v[] = {0.0f, 0.0f, 0.0f, 0.5f, 0.5f, 0.0f};

这两个点构成了一条线的两个端点。

定义一个GLuint类型的全局变量VBO来表示顶点缓冲，通常情况下，OpenGL对象都是用一个GLuint类型的变量表示。任何非零的无符号整数都可以作为缓冲区对象的标识符使用。可以任意选择一个有代表性的值，也可以让OpenGL负责分配和管理这些标识符。

    GLuint VBO;

可以使用`glGenBuffers`来创建缓冲区对象，OpenGL中，都是通过glGen*类型的函数产生各种各样的对象，这类函数有2个参数，第一个参数指定你要创建对象的数量，第二个参数是一个GLuint类型数组的地址，该地址中存放着driver分配给你的各种对象句柄。driver会保证将来再调用该函数不会产生相同的对象句柄，除非你调用函数glDeleteBuffers显示删除对象句柄。`glGenBuffers`函数的原型：

    void glGenBuffers(GLsizei n, GLuint *buffers);

使用：

    glGenBuffers(GL_ARRAY_BUFFER, VBO);

绑定和加载缓冲区对象
-----------------

OpenGL中，通常把一个对象绑定到一个target name，比如把VBO对象绑定到GL_ARRAY_BUFFER(表示缓冲是顶点数组，另一个常用的target name是GL_ELEMENT_ARRAY_BUFFER，表示索引数组)，然后在这个target name上执行命令，这些命令将会一直影响绑定的对象，除非我们把该target name绑定到一个新的对象，此时，在target name上执行命令，将会影响新绑定的对象。

    glBindBuffer(GL_ARRAY_BUFFER, VBO);

绑定对象后，我们开始准备顶点缓冲数据(往图形硬件拷贝顶点数组):

    glBufferData(GL_ARRAY_BUFFER, sizeof(v), v, GL_STATIC_DRAW);

`glBufferData`的函数原型：

    void glBufferData(GLenum target, GLsizeptr size, GLvoid *data, GLenum usage);

target可以是GL_ARRAY_BUFFER或者GL_ELEMENT_ARRAY_BUFFER等。size指定了顶点数组的大小(以字节为单位)。最后一个参数是用法提示如下：

    GL_DYNAMIC_DRAW: 存储在缓冲区对象中的顶点数组经常要更新，并且可能多次作为绘图的来源。这个提示告诉OpenGL实现把数据放置在更新开销不大的地方。
    GL_STATIC_DRAW:  数组很少更新，但可能多次作为绘图的来源。这个提示告诉OpenGL实现把数据放置在能够快速渲染但不需要快速更新的地方。
    GL_STREAM_DRAW:  数据极少变化，并且最多只有几次作为绘图的源数据。这个提示告诉OpenGL实现有一些时间敏感的数据（例如动画）将只使用一次，然后被替换。

上面的函数中，我们用v中的数据填充顶点缓冲，其中GL_STATIC_DRAW表示，这些顶点数据渲染过程中不会改变，相对应的是GL_DYNAMIC_DRAW，driver会根据这些状态对程序进行一定的优化。

使用顶点缓冲区对象渲染
-------------------

在这个例子中，我们没有使用shader，但是我们在顶点缓冲中装入了顶点位置，位置属性就作为顶点属性的index 0（注意：由于没有shader，我们此时用的是固定管线渲染)，所以必须打开它，否则不能使用，因为所有的顶点属性在使用前都必须打开。

    glEnableVertexAttribArray(0);

再一次绑定顶点缓冲：

    glBindBuffer(GL_ARRAY_BUFFER, VBO);

当然也可以不绑定，因为我们只有一个缓冲，前面已经绑定过了，但是在大型3D程序中，可能有很多顶点缓冲，就需要在渲染前，根据实际需要，切换绑定不同的顶点缓冲对象。

然后，使用`glVertexAttribPointer`函数告诉管线怎么解释顶点缓冲中的数据。第一个参数指定属性的索引，第二个参数是属性的数量(3，表示,x,y,z)，第三个参数是属性的数据类型，第四个参数是属性是否是归一化的，第五个参数是stride，表示属性的字节数目,指定连续顶点属性之间的偏移量。**如果为0，那么顶点属性会被理解为：它们是紧密排列在一起的。**初始值为0。我们只有一个属性，所以该值是0，如果我们有2个属性，一个位置，一个法向，假定它们都是3维float格式的向量，则这个参数要设置为(6*4)=24byte，最后一个参数offset，如果有2个属性，那么对于第一个属性，该值为0，没有偏移，对于第二个属性为第一个属性的字节数。

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);

我们调用draw函数，该函数是GPU开始工作的起始点，GPU将绑定draw函数的参数以及状态等数据，然后通过driver传递到GPU。OpenGL有几种形式的draw函数，通常它们可以划分为ordered draws和indexed draws。ordered draw函数就是把指定的顶点按语义顺序画一遍，比如你指定GL_TRIANGLES ，则0-2顶点为第一个三角形，3-5顶点为第二个三角形等等。而Index draws要通过索引缓冲来索引顶点数据(实际上，在硬件层次，没有无索引的draw实现，如果没有指定索引缓冲，硬件会按照顶点的顺序，生成一个和顶点顺序对应的索引)，这样可以重复利用顶点数据，比如我们可以用四个点来表示一个四边形(2个三角形组成)，其中2个点是2个三角形共享的，索引缓冲中的数据为顶点在顶点缓冲中的位置。

    glDrawArrays(GL_LINES, 0, 2);

我们调用DrawArrays函数画一条线，第一个参数指定体元语义，画的是线，第二个参数是第一个顶点的索引位置，第三个参数是要渲染的顶点的数量。

**不使用顶点属性的时候，记得要关闭它，以免引起未知的一些错误**：

    glDisableVertexAttribArray(0);

清除缓冲区对象
------------

完成了对缓冲区对象的操作之后，可以释放它的资源，并使它的标识符可以由其他缓冲区对象使用。为此，可以调用glDeleteBuffers()。被删除的当前绑定缓冲区对象的所有绑定都将重置为零。函数原型：

    void glDeleteBuffers(GLsizei n, const GLuint *buffers);

如果一个缓冲区对象是在绑定时删除的，这个对象的所有绑定都重置为默认的缓冲区对象，就像以0作为指定的缓冲区对象参数调用了glBindBuffer()一样。如果试图删除不存在的缓冲区对象或名称为0的缓冲区对象，这个操作将被忽略，并不会产生错误。

程序运行效果：

![OpenGL Plot Lines]({{site.url}}/resource/opengl_vbo/vbo_plot_lines.png)

参考
-----

1. [Ogldev tutorial 02： Hello Dot!](http://ogldev.atspace.co.uk/www/tutorial02/tutorial02.html)
