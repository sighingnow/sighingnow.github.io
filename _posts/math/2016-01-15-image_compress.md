---
title: 使用FFT和DCT进行图片压缩
author: Tao He
date: 2016-01-15
tag: [Signal Processing]
category: Math
layout: post
---

使用二维离散傅里叶变换或二维离散余弦变换可以得到图片的频率分布（频谱），在此基础上过滤调高频信号，然后进行离散傅里叶反变换或者离散余弦反变换
可以得到压缩后的图片。

<!--more-->

原理
----

二维傅里叶变换是傅里叶变换在二维定义域上的表达形式：

$$F(u,v) = \int_{-\infty}^{+\infty} \int_{-\infty}^{+\infty} f(x,y)e^{-j2\pi(ux+vy)} dx dy$$

图像、声音等信号是离散信号，对图像做二维离散傅里叶变换，可以得到图像的频率分布：

$$\begin{aligned}
F(k,l) &= \sum_{m=0}^{M-1} \sum_{n=0}^{N-1} f(m,n)e^{-j2\pi(km/M+ln/N)} \\
       &= \sum_{m=0}^{M-1} e^{-j2\pi\frac{km}{M}} \sum_{n=0}^{N-1} f(m,n) e^{-j2\pi\frac{ln}{N}}
\end{aligned}$$

对得到的频谱进行低通滤波，可以得到压缩后的图像，对得到的频谱进行低通滤波，可以得到图片的大致轮廓。例如下例：

![高通滤波和低通滤波对比]({{site.url}}/resource/image_compress/fft_edge.jpg)

从这三张图片中可以看出，低通滤波后，原来图像色彩变化比较剧烈的地方变得模糊，但于此同时，图像的大小也减少了很多，说明图片被压缩了，同时失真了。
而最后一张图片是经过高通滤波后的图片，图片中原来的轮廓位置被保留了下来，其他地方的信息都丢失了，通过对图像的处理我们得到了图像的边缘检测，如果
直接对灰度图进行处理，效果将会更加明显。

使用离散余弦变换，然后低通滤波，再进行离散余弦逆变换，同样对图片进行压缩。基于以上原理，使用Matlab实现对图片的压缩功能。实际操作中，使用FFT
进行离散傅里叶变换，以加快算法的效率，同时提高计算精度。

Matlab 代码
----------

### 使用FFT压缩的代码

~~~matlab
% fft_compress.m

% 作用：使用离散傅里叶变换对输入的 JPG 图片 image 按照指定的滤波比例 ratio 进行压缩
% 返回值：返回低通滤波之后的频率分布和压缩之后的图片
function [z, k] = fft_compress(image, ratio)

    % 对原 JPG 图片在三个颜色上分别做二维离散傅里叶变换，得到频率分布
    z(:,:,1) = fft2(image(:,:,1));
    z(:,:,2) = fft2(image(:,:,2));
    z(:,:,3) = fft2(image(:,:,3));

    % 获取图片的尺寸大小
    [a, b, ~] = size(image);

    % 低通滤波
    for i = 1 : a
        for j = 1 : b
            if (i + j > (a+b) * ratio)
                z(i, j, 1) = 0;
                z(i, j, 2) = 0;
                z(i, j, 3) = 0;
            end
        end
    end

    % 对过滤之后的结果在三个颜色上分别做进行二维反离散傅里叶变换
    k(:,:,1) = ifft2(z(:,:,1));
    k(:,:,2) = ifft2(z(:,:,2));
    k(:,:,3) = ifft2(z(:,:,3));

    % 类型转换，转换为 0-255 范围内的颜色值
    k = uint8(k);
end
~~~

### 使用DCT压缩的代码

~~~matlab
% dct_compress.m

% 作用：使用离散余弦变换对输入的 JPG 图片进行压缩
function [z, k] = dct_compress(image, ratio)

    % 对原 JPG 图片在三个颜色上分别做二维离散余弦变换
    z(:,:,1) = dct2(image(:,:,1));
    z(:,:,2) = dct2(image(:,:,2));
    z(:,:,3) = dct2(image(:,:,3));

    % 获取图片的尺寸大小
    [a, b, ~] = size(image);

    % 低通滤波
    for i = 1 : a
        for j = 1 : b
            if (i + j > (a+b) * ratio)
                z(i, j, 1) = 0;
                z(i, j, 2) = 0;
                z(i, j, 3) = 0;
            end
        end
    end

    % 对过滤之后的结果在三个颜色上分别做进行二维反离散余弦变换
    k(:,:,1) = idct2(z(:,:,1));
    k(:,:,2) = idct2(z(:,:,2));
    k(:,:,3) = idct2(z(:,:,3));

    % 类型转换，转换为 0-255 范围内的颜色值
    k = uint8(k);
end
~~~

### 绘图代码

调用这两个函数，对 JPG 格式的图片进行压缩，并绘制出效果图

~~~matlab
clear;

origin = imread('origin.jpg');

% 使用离散余弦变换压缩

[z, k] = dct_compress(origin, 0.6);
subplot(5, 2, 5);
imshow(z);
subplot(5, 2, 6);
imshow(k), title('DCT 比例 0.6');

% 使用离散傅里叶变换压缩

[z, k] = fft_compress(origin, 0.6);
subplot(5, 2, 5);
imshow(z);
subplot(5, 2, 6);
imshow(k), title('FFT 比例 0.6');
~~~

运行效果
------

### 压缩效果

从压缩前后的图片大小上可以很明显得看到对图片进行压缩的效果。下表是不同的滤波比例下，FFT和DCT压缩后的图片体积上与原图的比较。压缩后的图片如下文
的压缩效果图所示。

| 原图大小(KB)      | FFT压缩后的大小(KB)    | DCT压缩后的大小(KB) |
|:----------------:|:-------------------:|:-----------------:|
| 245              | 108                 | 129               |
| 245              | 89                  | 128               |
| 245              | 79                  | 122               |
| 245              | 71                  | 101               |

### 原图

用于压缩的图片原图(Figure 2)：

![原图]({{site.url}}/resource/image_compress/origin.jpg)

### FFT压缩效果图

通过快速傅里叶变换和快速傅里叶逆变换之后得到的效果图如(Figure 3)（左侧为高通滤波后的频率分布，右侧为按照不同的率波比例压缩后的图片）：

![FFT效果图]({{site.url}}/resource/image_compress/fft_result.jpg)

### DCT压缩效果图

通过离散余弦变换和离散余弦反变换之后得到的效果图(Figure 4)（左侧为高通滤波后的频率分布，右侧为按照不同的率波比例压缩后的图片）：

![DCT效果图]({{site.url}}/resource/image_compress/dct_result.jpg)

