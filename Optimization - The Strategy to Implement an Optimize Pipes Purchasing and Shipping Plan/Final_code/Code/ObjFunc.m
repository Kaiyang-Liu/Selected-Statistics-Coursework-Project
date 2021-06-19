function Total_cost = ObjFunc(X_x_r, Cost_matrix, K, D)

    %   Initialization
    %   x matrix denotes the pipes transportated from Si to Aj.
    x = X_x_r(1:7,1:15);
    %   r(rj) denotes the length of piplines paving in the right hand side of Aj
    %   lj = Dj-1 - rj-1. 
    r = X_x_r(8,1:15);
    
    %   Initialize the function value(total cost)
    Total_cost = 0;
    
    %   Calculate the cost of the transpotations from Si to Aj and also the materials cost of factory i.
    Total_cost = Total_cost + sum(sum(Cost_matrix .* x));

    %   Calculate the cost of the transpotations during paving piplines.
    %   Denote that these terms are the sums of the Arithmetic sequences.
    for j = 1:14
        
        Total_cost = Total_cost + (r(j)*(r(j)+1) + (D(j)-r(j)) * (D(j)-r(j)+1)) * 0.05 ;
    
    end
    
end