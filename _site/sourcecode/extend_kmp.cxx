#include <iostream>
#include <cstdio>
#include <algorithm>
#include <cstring>
using namespace std;

/**
 * Author: DHDave (buaahetao@gmai.com)
 */

void extendKmp(char T[], char S[], int next[], int extend[]) {
    int slen = strlen(S), tlen = strlen(T), a = 0;
    int len = min(slen, tlen);

    while (a < len && S[a] == T[a]) {
        a++;
    }

    extend[0] = a; a = 0;

    for (int k = 1; k <= slen; ++k) {
        int p = a+extend[a]-1;
        int l = next[k-a];

        if (k+l-1 >= p) {
            int j = p-k+1 > 0 ? p-k+1 : 0;

            while (j+k < slen && j < tlen && S[j+k] == T[j]) {
                ++j;
            }

            extend[k] = j;
            a = k;
        }
        else {
            extend[k] = l;
        }
    }
}

/**
 * Extend KMP algorithm.
 * This implemention is modified from the code in Liu YaQiong's Slide.
 */
void extendKmpLYQ(char pattern[], char str[], int next[], int extend[]) {
    int a(0), p(0), pLen(strlen(pattern)), sLen(strlen(str));

    for (int i = 0, j = -1; i < sLen; ++i, --j) {
        if (j < 0 || i + next[i-a] >= p) {
            if (j < 0) {
                j = 0, p = i;
            }

            while (p < sLen && j < pLen && str[p] == pattern[j]) {
                ++p, ++j;
            }

            extend[i] = j, a = i;
        }
        else {
            extend[i] = next[i-a];
        }
    }
}

int main(int argc, char* argv[]) {
    int extend[1000] = {0}, next[10000] = {0};
    char S[1000] = "abcdbcdedbcd";
    char T[1000] = "bcded";

    /**
    extendKmp(T, T, next, next);
    extendKmp(T, S, next, extend);
    */

    extendKmpLYQ(T, T, next, next);
    extendKmpLYQ(T, S, next, extend);

    return 0;
}

/* vim: set ts=4, sw = 4 */


