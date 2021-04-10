---
title: OpenGL 坐标变换
author: Tao He
date: 2015-08-07
tag: OpenGL
category: Graphics
layout: post
---

图形学中主要有三类坐标变换：

+ Translation Transformation
+ Rotation Transformation
+ Scaling Transformation

OpenGL中，既可以通过单独计算某一个维度的值来实现变换，也可以通过矩阵乘法来实现变换。使用矩阵乘法的方法更方便，也使用综合多个变换，在OpenGL Shader中，也有相应的硬件支持。

<!--more-->

OpenGL坐标系
-----------

OpenGL坐标系可分为世界坐标系和当前绘图坐标系。

世界坐标系：在OpenGL中，世界坐标系是以屏幕中心为原点(0, 0, 0)，且是始终不变的。你面对屏幕，你的右边是x正轴，上面是y正轴，屏幕指向你的为z正轴。长度单位这样来定：窗口范围按此单位恰好是(-1,-1)到(1,1)，即屏幕左下角坐标为（-1，-1），右上角坐标为（1,1）。**世界坐标系是右手坐标系。**

当前绘图坐标系：是绘制物体时的坐标系。程序刚初始化时，世界坐标系和当前绘图坐标系是重合的。当对当前绘图坐标系进行平移、伸缩、旋转变换之后，世界坐标系和当前绘图坐标系不再重合。注意，这里的平移旋转是将当前绘图坐标系看做一个整体在世界坐标系中进行旋转平移。然后，改变以后，再用`glVertex3f()`等绘图函数绘图时，都是在当前绘图坐标系进行绘图，所有的函数参数也都是相对当前绘图坐标系来讲的。

OpenGL中，使用右手坐标

+ 从左到右，x递增
+ 从下到上，y递增
+ 从远到近，z递增

固定管线的坐标变换
---------------

不使用Shader，OpenGL也提供了一系列的坐标变换函数来实现坐标变换：

+ `void glScalef(GLfloat  x,  GLfloat  y,  GLfloat  z);`

模型变换的目的是设置模型的位置和方向，例如可以对模型进行旋转、移动和缩放，或者联合这几种操作。这个函数表示模型在各轴上是如果进行缩放的。举个例子：

    glScalef (1.0, 2.0, 1.0);

表示y坐标值扩大两倍，这样原本方的物体就变成长的了。

+ `void glTranslatef(GLfloat  x,  GLfloat  y,  GLfloat  z);`

这个函数表示模型是怎样移动的。举个例子：

    glTranslatef(-1.0,0.0,-2.0);

表示物体沿x负方向移动1.0，沿z轴负方向移动2.0。所以就好像能看见侧面一样

+ `void glRotatef(GLfloat  angle,  GLfloat  x,  GLfloat  y,  GLfloat  z);`

angle表示旋转的角度（注意单位不是弧度），(x,y,z)表示转轴。举个例子：

    glRotatef(45.0, 0.0, 0.0, 1.0);

表示模型沿着(0,0,1)这个轴旋转45°。

OpenGL中有一个坐标变换矩阵栈(ModelView)，栈顶就是当前坐标变换矩阵，进入OpenGL管道的每个坐标(齐次坐标)都会先乘上这个矩阵，结果才是对应点在场景中的世界坐标。OpenGL中的坐标变换都是通过矩阵运算完成的，要注意的是**变换中的矩阵乘法是左乘**(左乘一个矩阵，就代表着在原有变换基础上继续变换)，而矩阵乘法与算术乘法不同，不符合交换律(万一不明白去看矩阵代数书好了)。OpenGL中还有一个有投影变换矩阵栈(Projection)，栈顶矩阵就是当前投影变换矩阵，负责将场景各坐标变换到眼坐标，由所得到的结果是裁剪后的场景部分，称为裁剪坐标。

矩阵切换函数：

    glMatrixMode(mode)

参数：

+ `GL_MODELVIEWING`: 坐标变换矩阵栈
+ `GL_PROJECTION`: 投影变换矩阵栈

本命令执行后参数所指矩阵栈就成为当前矩阵栈，以后的矩阵栈操纵命令将作用于它。 矩阵栈操纵命令有：

+ `glPushMatrix()`: 当前矩阵入栈，这时矩阵栈将栈顶值压入栈。
+ `glPopMatrix()`: 栈顶出栈，通常与上一条命令配合使用。
+ `glLoadIdentity()`: 将栈顶设为不变矩阵(就是对角线全为1其它为0的那个)。
+ `glMultMatrix(M)`: 将栈顶`T`设为 `M * T`。

另一个重要的函数：

    void gluLookAt(GLdouble eyex, GLdouble eyey, GLdouble eyez,
                   GLdouble centerx, GLdouble centery, GLdouble centerz,
                   GLdouble upx,GLdouble upy,GLdouble upz);

第一组eyex, eyey,eyez 相机在世界坐标的位置；第二组centerx,centery,centerz 相机镜头对准的物体在世界坐标的位置；第三组upx,upy,upz 相机向上的方向在世界坐标中的方向。你把相机想象成为你自己的脑袋：第一组数据就是脑袋的位置；第二组数据就是眼睛看的物体的位置；第三组就是头顶朝向的方向（因为你可以歪着头看同一个物体）。

矩阵乘法和坐标变换
---------------

+ 平移：

![translation]({{site.url}}/resource/opengl_transformation/opengl_translation.jpg)

+ 旋转：

绕一个轴转意味着这个轴的法平面不动，OpenGL里，一般需要做的是绕 x, y, z 轴旋转：

![rotation]({{site.url}}/resource/opengl_transformation/opengl_rotation.png)

+ 缩放：

缩放矩阵是一个对角矩阵，如下图所示，a, b,c分别表示在x，y，z三个轴上的缩放比例:

![scaling matrix]({{site.url}}/resource/opengl_transformation/opengl_scaling_3.png)

把它扩展成4x4矩阵，以便和齐次坐标匹配:

![scaling]({{site.url}}/resource/opengl_transformation/opengl_scaling_4.png)

在Shader中使用变换
----------------

首先，需要在vertex shader中建立一个世界矩阵，然后使用这个矩阵进行变换：

    #version 400
    layout (location = 0) in vec3 Position;
    uniform mat4 gWorld;
    void main()
    {
        gl_Position = gWorld * vec4(Position, 1.0);
    }

在C/C++程序中准备一个4x4的矩阵，此处使用一个平移矩阵：

    mat4x4f translation = {
        1.0f, 0.0f, 0.0f, sinf(Scale),
        0.0f, 1.0f, 0.0f, 0.0f,
        0.0f, 0.0f, 1.0f, 0.0f,
        0.0f, 0.0f, 0.0f, 1.0f };

接下来，使用`glUniformMatrix4fv`函数为Uniform变量赋值：

    glUniformMatrix4fv(gWorldLocation, 1, GL_TRUE, &translation[0][0]);

第一个参数是shader uniform变量的位置索引，第二个参数是要更新的矩阵的数量，我们只有一个矩阵，所以其值是1，通过这个参数，我们可以同时给多个矩阵赋值。第三个参数指定矩阵是行主序还是列主序，行主序意思是矩阵在数组中是按一行一行存储的，列主序则表示矩阵在数组中是一列一列存储的，c++默认是行主序的。第四个参数矩阵在内存中的起始地址。

也可以把多个变换用矩阵合成后，使用`glUniformMatrix4fv`传递给Shader，或者设置多个Uniform变量，通过Shader进行矩阵乘法的运行。与固定管线相比，Shader确实会灵活很多。运行效果：

<video controls="controls" width="800" height="480">
    Your browser does not support the video tag.
    <source src="{{site.url}}/resource/opengl_transformation/opengl_ransformation.webm" type="video/webm">
    <source src="{{site.url}}/resource/opengl_transformation/opengl_ransformation.mp4" type="video/mp4">
</video>

坐标变换顺序
----------

**正确的坐标变换顺序：先缩放；再调整方向；最后平移。**

例如，假设有个船的模型（为简化问题，略去旋转）：

* 错误做法：
    - 按(10, 0, 0)平移船体。船体中心目前距离原点10个单位。
    - 将船体放大2倍。以原点为参照，每个坐标都变成原来的2倍，这就出问题了。最后得到的是一艘放大的船，但其中心位于2*10=20。这并非预期的结果。

* 正确做法：
    - 将船体放大2倍，得到一艘中心位于原点的大船。
    - 平移船体。船大小不变，移动距离也正确。

每一步坐标变换都是在原来的举证的基础上**左乘**变换矩阵！经过这些缩放、旋转、平移得到的矩阵称为**模型(model)矩阵**。

完整代码链接：[translation.cpp]({{site.url}}/resource/opengl_transformation/06-07-08_transformation.cpp)

