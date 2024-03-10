import numpy as np
import matplotlib.pyplot as plt
from utils import *
import copy
import math
x_train = np.array([
    [2104,    5,    1,   45],
    [1416,    3,    2,   40],
    [852,    2,    1,   35]
])
y_train = np.array([460, 232, 178])




def compute_cost(x,y,w,b):
    m = x.shape[0]
    total_cost = 0

    for i in range(m):
        f_wb = np.dot(x[i],w) +b
        total_cost += (f_wb - y[i])**2
    total_cost = total_cost/(2*m)
    return total_cost
def compute_gradient(x, y, w, b):
    m,n = x.shape
    
    dj_dw = np.zeros((n,))
    dj_db = 0.
    for i in range(m):
        f_wb = np.dot(x[i],w) + b 
        dj_db += (f_wb - y[i])
        for j in range(n):
            dj_dw[j] += (f_wb - y[i])*x[i,j]
        dj_db += (f_wb - y[i])
    dj_dw = dj_dw/m
    dj_db = dj_db/m
    return dj_dw, dj_db
def gradient_descent(x,y,w_in,b_in,cost_function,gradient_function,alpha,num_iters):
    j_history = []
    w = copy.deepcopy(w_in)
    b = b_in
    for i in range(num_iters):
        dj_dw, dj_db = gradient_function(x,y,w,b)

        w = w - alpha*dj_dw
        b = b - alpha*dj_db

        if i<83000002:      # prevent resource exhaustion 
            j_history.append( cost_function(x, y, w, b))
        # Print cost every at intervals 10 times or as many iterations if < 10
        if i% math.ceil(num_iters / 10) == 0:
            print(f"Iteration {i:4d}: Cost {j_history[-1]:8.2f}   ")
        
    return w, b, j_history

m,n = x_train.shape 
initial_w = np.zeros(4)
initial_b = 0.
iterations = 83000000
alpha = 5.0e-7
w_final, b_final, J_hist = gradient_descent(x_train, y_train, initial_w, initial_b,
                                                    compute_cost, compute_gradient, 
                                                    alpha, iterations)
print(f"b,w found by gradient descent: {b_final:0.2f},{w_final} ")
# feature scaling



        
