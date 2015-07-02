def P10(n):
    r = int(n**0.5)
#   assert r*r <= n and (r+1)**2 > n
    V = [n//i for i in range(1, r+1)]
#   print V
    V += list(range(V[-1]-1, 0, -1))
#   print V
    S = {i: i*(i+1)//2-1 for i in V}
#   print S
    for p in range(2, r+1):
        if S[p] > S[p-1]:  # p is prime
            sp = S[p-1]  # sum of primes smaller than p
            p2 = p*p
            for v in V:
                if v < p2:
                    break
                S[v] -= p*(S[v//p] - sp)
    return S[n]

