#include <iostream>
#include <cstdio>
#include <algorithm>
using namespace std;

/**
 * Author: DHDave (buaahetao@gmai.com)
 */

struct Node {
    int data;
    int height;
    struct Node *left, *right;
    Node(int data): data(data), height(1), left(NULL), right(NULL) {}
}; 

inline int getH(Node *node) {
    if(node) {
        return node->height;
    }
    else {
        return 0;
    }
}

// LL.
void singleRotateLeft(Node *&node) {
    Node *k = node->left;
    node->left = k->right;
    k->right = node;
    node->height = max(getH(node->left), getH(node->right))+1;
    k->height = max(getH(k->left), getH(k->right))+1;
    node = k;
}

// RR.
void singleRotateRight(Node *&node) {
    Node *k = node->right;
    node->right = k->left;
    k->left = node;
    node->height = max(getH(node->left), getH(node->right))+1;
    k->height = max(getH(k->left), getH(k->right))+1;
    node = k;
}

// LR.
inline void doubleRotateLeft(Node *&node) {
    singleRotateRight(node->left);
    singleRotateLeft(node);
}

// RL.
inline void doubleRotateRight(Node *&node) {
    singleRotateLeft(node->right);
    singleRotateRight(node);
}


// insert node.
void insert(Node * &pnew, int data) 
{
    if(pnew == NULL) {
        pnew = new Node(data);
        return;
    }
    
    if(data < pnew->data) {
        insert(pnew->left, data);
        if(getH(pnew->left)-getH(pnew->right) == 2) {
            if(data < pnew->left->data) {
                singleRotateLeft(pnew);
            }
            else {
                doubleRotateLeft(pnew);
            }
        }
    }
    else {
        insert(pnew->right, data);
        if(getH(pnew->right)-getH(pnew->left) == 2) {
            if(data >= pnew->right->data) {
                singleRotateRight(pnew);
            }
            else {
                doubleRotateRight(pnew);
            }
        }
    }
    pnew->height = max(getH(pnew->left), getH(pnew->right)) + 1;
}


// rejust node.
inline void rotateNode(Node *&node) {
    if(getH(node->left) - getH(node->right) == 2) {
        if(getH(node->left->left) >= getH(node->left->right)) {
            singleRotateLeft(node);
        }
        else {
            doubleRotateLeft(node);
        }
    }
    
    if(getH(node->right) - getH(node->left) == 2) {
        if(getH(node->right->right) >= getH(node->right->left)) {
            singleRotateRight(node);
        }
        else {
            doubleRotateRight(node);
        }
    }
}

// drop node.
void drop(Node *&node, int value) {
    if(!node) { // node == NULL
        return;
    }
    if(node->data == value) {
        Node *tmp = NULL;
        if(node->right == NULL) { // 右子树为空，直接删除
            tmp = node;
            node = node->left;
            delete(tmp);
        }
        else { // 右子树不为空，找到node->right 的最左儿子替代 node
            tmp = node->right;
            while(tmp->left) {
                tmp = tmp->left;
            }
            node->data = tmp->data;
            drop(node->right, node->data); // 调整子节点
            node->height = max(getH(node->left), getH(node->right)) + 1;
        }
        return;
    }
    else if(value > node->data) {
        drop(node->right, value);
    }
    else {
        drop(node->left, value);
    }

    /* 删除完元素之后重新调整平衡. */
    node->height = max(getH(node->left), getH(node->right)) + 1;
    if(node->left != NULL) {
        rotateNode(node->left);
    }
    if(node->right != NULL) {
        rotateNode(node->right);
    }
    rotateNode(node);
}

inline void printTree(struct Node *root) {
    if(root) {
        printTree(root->left);
        printf("%8d %8d\n", root->data, root->height);
        printTree(root->right);
    }
}

int main(int argc, char *argv[])
{
	int data[] = {
            1, 2, 3, 4, 5, 
            10, 9, 8, 7, 6, 
            11, 12, 13, 14, 15, 
            20, 19, 18, 17, 16};
    struct Node *root = NULL;
    for(int x: data) {
        insert(root, x);
    }
    printTree(root);
    printf("----------------\n");
    for(int x = 1; x <= 20; ++x) {
        drop(root, x);
        printTree(root);
        printf("\n");
    }
    printTree(root);
    return 0;
}

/* vim: set ts=4, sw = 4 */


