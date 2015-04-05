def lflbab_factor(x,y):
    if x not in {0,1} or y not in {0,1}:
        return 0.0
    elif x == y and x == 1:
        return 3
    elif x == y and x == 0:
        return 2.0
    else:
        return 1.0
        
def lflbab_factor_product(x1,x2,x3, x4, x5):
    return lflbab_factor(x1,x2)*lflbab_factor(x2,x3)*lflbab_factor(x3,x4)*lflbab_factor(x4,x5)

def lflbab_marginals2(x2):
    acc = 0
    for x1 in range(0,2):
        for x3 in range(0,2):
            for x4 in range(0,2):
                for x5 in range(0,2):
                    acc += lflbab_factor_product(x1, x2, x3, x4, x5)
    return acc
    
def main():
    z0 = lflbab_marginals2(0)
    z1 = lflbab_marginals2(1)
    print([z0 / (z0 + z1), z1 / (z0 + z1)])
    
if __name__ == '__main__':
    main()
        
    