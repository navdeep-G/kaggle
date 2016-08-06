import numpy as np
import pandas as pd
import math

def fit_coef(col_lst):
    global train, test
    merge = None

    def cal_x_y(col_lst):
        col1 = col_lst[0]
        col2 = col_lst[1]

        name1 = '_'.join([''.join(x.split('_')) for x in col1])
        mean_train = train.groupby(col1)['log_demand'].mean().reset_index(name=name1)
        merge = pd.merge(test, mean_train, how='inner', on=col1)

        name2 = '_'.join([''.join(x.split('_')) for x in col2])
        mean_train = train.groupby(col2)['log_demand'].mean().reset_index(name=name2)
        merge = pd.merge(merge, mean_train, how='inner', on=col2)

        x1 = merge[name1].apply(np.expm1)
        x2 = merge[name2].apply(np.expm1)
        y = merge['log_demand']
        return x1, x2, y

    x1, x2, y = cal_x_y(col_lst)
    n = x1.size

    def f(v, x1, x2, y):
        a = v[0]
        b = v[1]
        c = v[2]
        square = (np.log1p(a*x1 + b*x2 + c) - y)**2
        return square.sum() / n

    def Df(v, x1, x2, y):
        a = v[0]
        b = v[1]
        c = v[2]
        t = a*x1 + b*x2 + c
        tmp = 2*(np.log1p(t) - y) / (1+t) / n
        d1 = tmp * x1
        d2 = tmp * x2
        d0 = tmp
        return np.array([d1.sum(), d2.sum(), d0.sum()])

    v0 = np.random.rand(3)
    v = None
    f1 = None
    max_iter = 1000
    eps = 1.0e-8

    # using gradient descent method to fit coefs
    for i in range(max_iter):
        g = Df(v0, x1, x2, y)
        gl2 = (g**2).sum()
        f0 = f(v0, x1, x2, y)

        alpha = 1
        c = 0.5
        rho = 0.5
        # line search
        while f(v0 - alpha * g, x1, x2, y) > f0 - c * alpha * gl2:
            alpha = alpha * rho

        v = v0 - g * alpha
        f1 = f(v, x1, x2, y)
        if abs(math.sqrt(f0)-math.sqrt(f1)) < eps:
            break
        v0 = v

    return v, math.sqrt(f1)


if __name__ == "__main__":
    cols = ['Cliente_ID', 'Producto_ID', 'Ruta_SAK', 'Agencia_ID', 'Canal_ID', 'Demanda_uni_equil']
    train = pd.read_csv('../../data/train_3_8.csv')[cols]
    test = pd.read_csv('../../data/train_9.csv')[cols]
    train['log_demand'] = train['Demanda_uni_equil'].apply(np.log1p)
    test['log_demand'] = test['Demanda_uni_equil'].apply(np.log1p)

    col_lst = [
            ['Producto_ID', 'Cliente_ID', 'Agencia_ID'],
            ['Producto_ID', 'Ruta_SAK'],
            ]
    f = open('coef.csv', 'a')
    f.write("coef,log_rmse\n")
    v, sqrt_f1 = fit_coef(col_lst)
    f.write("%s,%s,%s\n" % (col_lst, v, sqrt_f1))
    f.close()