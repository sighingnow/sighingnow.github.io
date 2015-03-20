#include <cstdio>
#include <cstring>
using namespace std;
/*******************************************
 * Aho-Corasick automation
 ********************************************/

const int kind = 26;

struct Node{
    Node *fail;
    Node *next[kind];
    int count; //记录是否为该单词的最后一个字母, count 值为以该节点为结尾的单词数

    Node() {
        this->fail = NULL;
        this->count = 0;
        memset(next, 0, sizeof(next[0]) * kind);
    }
};

int head = 0, tail = 0; //bfs队列的首尾
Node *bfs_queue[50000]; // bfs queue

// 构造trie(字母树)
void insert(char pattern[], Node *root) {
    Node *p = root;
    int i = 0, index;
    while(pattern[i]) {
        index = pattern[i] - 'a';
        if(p->next[index] == NULL) {
            p->next[index] = new Node();
        }
        p = p->next[index];
        ++i;
    }
    (p->count)++;
}

void build_ac_auto(Node *root) {
    root->fail = NULL;
    bfs_queue[head++] = root;
    while(head != tail) {
        Node *tmp = bfs_queue[tail++];
        Node *p = NULL;
        for(int i = 0; i < 26; ++i) {
            if(tmp->next[i] != NULL) {
                if(tmp == root) {
                    tmp->next[i]->fail = root;
                }
                else {
                    p = tmp->fail;
                    while(p != NULL) {
                        if(p->next[i] != NULL) {
                            tmp->next[i]->fail = p->next[i];
                            break;
                        }
                        p = p->fail;
                    }
                    if(p == NULL) {
                        tmp->next[i]->fail = root;
                    }
                }
                bfs_queue[head++] = tmp->next[i];
            }
        }
    }
}

int query(Node *root, char str[]) {
    int i = 0, cnt = 0, index;
    Node *p = root;
    while(str[i]) {
        index = str[i] - 'a';
        while(p->next[index] == NULL && p != root) {
            p = p->fail;
        }
        p = p->next[index];
        p = (p == NULL) ? root : p;
        Node *tmp = p;
        while(tmp != root) {
            cnt += tmp->count;
            tmp = tmp->fail;
        }
        /***************************************
         * 若每个模式串的出现只记一次，则为
         * while(tmp != root && tmp->count != -1) {
         *     cnt += tmp->count;
         *     tmp = tmp->tail;
         *     tmp->count = -1;
         * }
         * ***************************************/
        i++;
    }
    return cnt;
}

int main(int argc, char *argv[]) {
    
    int n;
    char pattern[100];
    scanf("%d", &n);
    getchar(); //过滤回车
    Node *root = new Node();
    char str[1000000]; //主串
    while(n--) {
        scanf("%s", pattern);
        insert(pattern, root);
    }
    build_ac_auto(root);
    scanf("%s", str);
    printf("%d\n", query(root, str));
    return 0;
}
