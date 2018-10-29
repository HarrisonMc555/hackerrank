#!/usr/bin/env python3

import math

def valid_bst_preorder(preorder):
    if not preorder:
        return True
    x = preorder[0]
    min_val = -1*math.inf
    max_val = x
    stack = [x]
    for v in preorder[1:]:
        if v < min_val:
            return False
        while v >= max_val:
            min_val = max_val
            if stack:
                max_val = stack.pop(-1)
            else:
                max_val = math.inf
        stack.append(max_val)
        max_val = v
    return True


def main():
    T = int(input())
    for _ in range(T):
        _ = int(input())
        preorder = [int(s) for s in input().split()]
        if valid_bst_preorder(preorder):
            print('YES')
        else:
            print('NO')


if __name__ == '__main__':
    main()
