def next(rec,f,s):
    print("r:%d, f: %d, s: %d"%(len(rec), f, s))
    ns = rec + [int(x) for x in str(rec[f] + rec[s])]
    def ni(i, v):
        ci = i + v + 1
        if (ci >= len(ns)):
            print("wrap")
            ci = ci % len(ns)
        elif (ci < 0):
            print("what")
            ci = ci + len(ns)
        return ci
    return (ns, ni(f,rec[f]),ni(s,rec[s]))

def comp2(ini, limit):
    st = ini
    ll = len(limit)
    ending = st[0][-ll:]
    i=0
    while ending != limit:
        st = next(st[0], st[1], st[2])
        ending = st[0][-ll:]
        i = i + 1
    return len(st[0]) - ll

re2 = comp2(([3,7],0,1), [0,7,7,2,0,1])
print("start")
print(re2)
print("done")
