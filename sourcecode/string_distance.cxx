#include <iostream>
#include <cstdio>
#include <algorithm>
#include <cstring>
using namespace std;

/**
 * author: sighingnow
 */

/*
 * 注意：为方便DP，字符串从索引为 1 的位置开始。
 */
int getDistance(char A[], char B[]) {
    int Alen(strlen(A+1)), Blen(strlen(B+1));
    int dis[Alen+5][Blen+5];
    memset(dis, 0, sizeof(dis[0][0])*(Alen+5)*(Blen+5));
    for(int i = 1; i <= Alen; ++i) {
        for(int j = 1; j <= Blen; ++j) {
            if(A[i] == B[j]) {
                dis[i][j] = dis[i-1][j-1];
            }
            else {
                dis[i][j] = min(dis[i-1][j-1]+1,
                        min(dis[i][j-1], dis[i-1][j])+1);
            }
        }
    }
    return dis[Alen][Blen];
}

int main(int argc, char *argv[])
{
	char A[1000], B[1000];
    scanf("%s %s", A+1, B+1);
    cout << getDistance(A, B) << endl;
	return 0;
}

/* vim: set ts=4, sw = 4 */


