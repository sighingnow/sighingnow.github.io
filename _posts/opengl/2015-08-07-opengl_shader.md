---
title: OpenGL Shader
author: Tao He
date: 2015-08-07
tag: OpenGL
category: Graphics
layout: post
---

计算机图形系统的发展以提供更快的图形数据处理和更真实的视觉效果为目标！

固定管线和可编程管线
-----------------

图形处理的pipeline解决的问题都一样，所谓固定管线（fixed function pipeline），是说芯片上一组电路已经固定实现了特定的运算功能，程序能做的只是提供场景数据以及微调运算功能的参数。固定渲染管线就是只可配置（configurable）的管线，实现不同效果就好像在电路中打开不同的开关，这样做比较直接，但是它是基于一个基本假设，就是做硬件能预先确定应用会需要哪些能力并提前做进去，如果你要的算法没在里面怎么办？只能各种绕，有时直接就做不了。例如texture stage的设置就是最复杂的部分，而stage数量非常有限（Direct3D 9只有8个），想要组合出各种各样的效果就会非常复杂。

可编程渲染管线（programmable pipeline）就直接把电路做成相对通用的流处理单元，让写软件的人以shader的形式给这些浮点运算能力超强的单元安排工作。可编程渲染管线把很多部分从可配置改为可编程（programmable），各种效果及他们的组合可以通过一般编程的方式实现，自由度高得多，而且不需那么伤脑筋。最通常的应用就是自行实现光照和材质，可用上各种光照和反射模型，也可把一些运算放在顶点上算，一些运算放在像素上算。

<!--more-->

Shader
------

基于光栅化（Rasterization）图形管线是这样的：

![Pipeline]({{site.url}}/resource/opengl_shader/pipeline.jpg)

图中下部虚线部分数据流向代表固定图形管线。应用程序通过点、线、多边形等几何图元构建出物理模型或可视化的数据结构，这些模型最后表示成具有对象空间坐标、法向量、颜色、纹理坐标等属性的顶点集（Vertex）。对顶点进行坐标变换、光照计算后，图元装配和光栅化操作对几何图元进行纹理和颜色的插值，生成和窗口屏幕像素相对应的片元集（Fragment）。片元集具有窗口坐标、颜色、纹理坐标、雾化坐标等属性，每个片元经过纹理应用颜色混合以及雾化等操作后计算出片元最终颜色和深度。通过模板、深度、透明混合等测试的片元最终写入帧缓存中显示出来。

Shader Model（在3D图形领域常被简称SM）就是“优化渲染引擎模式”。事实上，Shader（着色器）是一段能够针对3D对象进行操作、并被GPU所执行的程序。OpenGL ES 的管线工作模式图：

![Shader Pipeline]({{site.url}}/resource/opengl_shader/shader_pipeline.jpg)

在OpenGL中，顶点shader叫做vertex shader(vs)，像素shader叫做fragment shader(fs)。通过Shader，图形开发人员可以对渲染管线中的顶点运算和像素运算分别进行编程处理了，而无须象以前那样套用一些固定函数，通过设置参数来控制管线。

OpenGL的shader管线
-----------------

OpenGL中的Shader管线框图如下：

![OpenGL programmable pipeline]({{site.url}}/resource/opengl_shader/opengl_shader_pipeline.png)

几个阶段：

1. 顶点处理(vertex processor)，该阶段主要是对每个顶点执行shader操作，顶点数量在draw函数中指定。顶点shader中没有任何体元语义的内容，仅是针对顶点的操作，比如坐标空间变化，纹理坐标变化等等。每个顶点都必须执行顶点shader，不能跳过该阶段，执行完顶点shader后，顶点进入下一个阶段。

2. 几何处理(geometry processor)，在该阶段，顶点的邻接关系以及体元语义都被传入shader，在几何shader中，不仅仅处理顶点本身，还要考虑很多附加的信息。几何shader甚至能改变输出体元语义类型，比如输入体元是一系列单独的点(point list体元语义)，而输出体元则是三角形或者两个三角形组成的四边形等等， 甚至我们还能在几何shader中输入多个顶点，对于每个顶点输出不同语义的体元。

3. clipper阶段，或者称作裁剪阶段，这是一个固定管线模块，它将用裁剪空间的6个面对体元进行裁剪操作，裁剪空间外的部分将会被移去，这时可能会产生新的顶点和新的三角形，用户也可以定义自己的裁剪平面进行裁剪操作，裁剪后的三角形将会被传到光栅化阶段。[注：在clipper之前，会有PA阶段，就是顶点shader或几何shader处理过的顶点被重新装配成三角形]

4. 光栅化阶段和片元操作阶段，clipper后的三角形会先被光栅化，产生很多的fragment(片元)，接着执行片元shader，在片元shader中，可能会装入纹理，从而产生最终的像素颜色。 [fragment可以理解为带sample、深度信息的像素]

OpenGL的顶点shader、几何shader以及片元shader都是**可选的**，如果没有指定它们，则会执行**缺省**的功能。

opengl中编写shader程序
--------------------

shader管理类似于创建C/C++程序，首先写shader代码，把代码放在一个文本文件或者一个字符串中，然后编译该shader代码，把编译后的shader代码放到各个shader对象中，接着把shader对象链接到程序中，最后把shader送到GPU中去。

创建一个shader程序对象:

    GLuint ShaderProg = glCreateProgram();

然后创建Shader对象并将Shader对象attach到ShaderProg上去：

    GLuint ShaderObj = glCreateShader(ShaderType);

shader类型：

+ GL_VERTEX_SHADER
+ GL_FRAGMENT_SHADER

指定shader源代码和编译shader的函数对于这两种类型的shader来说是一样的。**如果只指定一个shader对象，比如只有vertex shader，那么fragment shader则使用固定管线的功能**。

编译shader之前，我们首先要通过glShaderSource函数指定shader的源代码，该函数可以通过字符指针数组（实际上是二维指针const GLchar ** ，每个元素都是一个字符指针，指向相应的源代码）指定多个shader源代码。该函数第一个参数是shader对象，第二个参数是个整数，指定字符指针数组中元素的个数，即多少个源代码，第三个参数为字符指针数组地址，第四个参数是个整数指针数组，和shader字符指针数组对应，它指定每个shader源代码的字符数量。

~~~cpp
const GLchar *p[1];
p[0] = pShaderText;
GLint Lengths[1];
Lengths[0] = strlen(pShaderText);
glShaderSource(ShaderObj, 1, p, Lengths);
~~~

接下来编译shader对象：

    glCompileShader(ShaderObj);

最终把编译的shader对象和shader程序对象绑定起来：

    glAttachShader(ShaderProg, ShaderObj);

通常编译shader的时候可能会碰到各种错误，这时候我们可以通过`glGetShaderiv`和`glGetShaderInfoLog`得到编译状态和错误信息，便于调试shader代码。一个例子：

~~~cpp
GLint success;
glGetShaderiv(ShaderObj, GL_COMPILE_STATUS, &success);
if (!success) {
    GLchar InfoLog[1024];
    glGetShaderInfoLog(ShaderObj, sizeof(InfoLog), NULL, InfoLog);
    fprintf(stderr, "Error compiling shader type %d: '%s'\n", ShaderType, InfoLog);
}
~~~

绑定之后是链接操作，链接操作之后，我们可以通过函数glDeleteShader释放中间shader对象。然后，检测shader链接时候是否有错误：

链接：

    glLinkProgram(ShaderProg);

检验是否出错：

~~~cpp
glGetProgramiv(ShaderProg, GL_LINK_STATUS, &success);
if (Success == 0) {
    glGetProgramInfoLog(ShaderProg, sizeof(ErrorLog), NULL, ErrorLog);
    fprintf(stderr, "Error linking shader program: '%s'\n", ErrorLog);
}
~~~

一般来说，这些检查在release的代码里是不需要的，删去还能提升性能。

最后，验证shader程序对象的有效性（链接检测基于shader绑定，而验证有效性则是验证程序能否在现在的管线上执行）。把链接好的程序对象送到shader管线。**这个shader将对随后的所有draw有效**，除非你用另一个shader程序对象代替它或者通过设置glUseProgram(NULL)禁止它(此时会打开固定管线功能)。

~~~cpp
glValidateProgram(ShaderProg);
glUseProgram(ShaderProg);
~~~

Shader的代码
-----------

点元Shader的代码：

    #version 400
    layout (location = 0) in vec3 Position;
    void main()
    {
        gl_Position = vec4(0.5 * Position.x, 0.5 * Position.y, Position.z, 1.0);
    }

`#version 400`表示使用4.0版本的GLSL（If the compiler does not support it it will emit an error）。layout (location = 0)在顶点buffer和顶点属性名字之间创建了一个绑定关系，属性名字是Position，属性值是一个三维坐标向量。**location指定该属性在顶点buffer中的位置**，我们必须要让编译器知道，顶点的那个属性和顶点buffer中的那个位置对应起来。通常有2种方法：

+ 例如在shader中指定的location = 0，在这种情况下，在cpp源代码中通过硬编码的方式，指定shader属性，比如glVertexAttributePointer(0)
+ 在shader代码中简单声明in vec3 Position，在应用程序中，通过函数glGetAttribLocation在运行时查找属性位置，这时我们要利用glGetAttribLocation的返回值，而不是硬编码。

多个shader对象链接在一起形成最终的shader，但是对每种类型shader(VS,GS,FS)来说,代码中只能有一个main函数，这个函数作为该shader的执行入口点，例如，我们能够创建一个shader库文件，其中包含计算光照的一些函数，然后把它链接到没有main函数的shader中去。

最后，`gl_Position`(XYZW)是OpenGL Shader的一个内置的变量，用来改变顶点位置。

+ W = 0 means a vector.
+ W != 0 means a point.

> In homogeneous coordinates, points have a w of 1 and vectors have a w of 0. So a point minus a point is a vector and so on. Note that valid values for w are just 0 and 1, so it doesn't make sense to add a point and a point.

片元shader的代码：

    #version 400
    out vec4 FragColor;
    void main()
    {
        FragColor = vec4(1.0, 0.0, 0.0, 1.0);
    }

片元shader的功能就是输出片元最终的颜色值。输出颜色最终通过宣布的out变量FragColor来完成，FragColor向量的四个分量分别表示R,G,B,A值，该值最终被写入framebuffer。`vec4(1.0, 0.0, 0.0, 1.0)`表示指定颜色为红色。

这儿也可以不指定输出，直接使用OpenGL Shader的一个内置的变量`gl_FragColor`(RGBA)。

    #version 400
    void main()
    {
        gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
    }

一致变量(Uniform Variables)
--------------------------

属性变量与一致变量之间的区别：属性变量包含顶点特性数据所以每次调用shader都会从顶点缓冲区中重新导入新的数据，而一致变量中的值在整个绘制过程中都保持不变。这意味着我们在绘制过程之前就为一致变量赋值并且在顶点着色器的每次调用中都可以访问这个相同的值。一致变量对于保存光照参数（光源位置和光照方向等），变换矩阵、纹理对象句柄等都是非常有用的。

GLUT不会重复调用我们的渲染函数，只有发生一些特殊事件的时候，才会执行渲染操作，比如窗口最大化、最小化，当前窗口被别的窗口遮挡等等。可以注册一个idle回调函数，把渲染函数放在该函数中，或者直接把渲染函数注册成idle函数，在GLUT不接受windows系统事件时，idle函数会重复执行，这样结合idle函数和变化的uniform变量，我们就可以实现动画的效果。

实现一个基本的IdleCallback函数：

    static void IdleCB()
    {
        sleep(10);
        glutPostRedisplay();
    }

然后使用`glutIdleFunc`绑定:

    glutIdleFunc(RenderSceneCB);

渲染函数的末尾要加上glutPostRedisplay()函数调用，否则的话idle函数会反复执行，但渲染函数却没有，glutPostRedisplay()会重绘当前显示窗口，并保证下一次glut消息循环中，渲染函数会被调用。

在Vertex Shader中定义一个Uniform变量，并让这个变量发挥作用：

    uniform float gScale;
    void main() {
        gl_Position = vec4(gScale * Position.x, gScale * Position.y, Position.z, 1.0);
    }"

**uniform变量在着色器中是只读的，它的值只能在着色器之外（我们的应用中）进行修改。uniform变量无法手动绑定，只能由程序对象在链接时自动绑定到uniform索引。之后，使用OpenGL函数glGetUniformLocation(programObject, name)获取绑定的uniform索引。**通过查询shader程序对象，我们能够得到uniform变量的位置，该位置和该uniform变量在shader代码中的位置一致。在shader内部访问uniform变量都是通过这个索引来实现的，应用程序则是通过`glGetUniformLocation`函数来访问相应的uniform变量，函数的参数为shader程序句柄以及uniform变量名字。函数调用成功，则会返回索引值，失败的话返回-1。

    GLuint gScaleLocation = glGetUniformLocation(ShaderProg, "gScale");
    assert(gScaleLocation != 0xFFFFFFFF);

然后在IdleCB函数中动态调整gScale的值，从而实现动画：

~~~cpp
static void IdleCB()
{
    static float Scale = 0.0f;
    Scale += 0.01f;
    glUniform1f(gScaleLocation, sinf(Scale));
    sleep(10);
    glutPostRedisplay();
}
~~~

源代码：[OpenGL_Shader.cpp]({{site.url}}/resource/opengl-shader/04_shader.cpp), [OpenGL_Uniform_Variable.cpp]({{site.url}}/resource/opengl-shader/05_uniform_variables.cpp)

