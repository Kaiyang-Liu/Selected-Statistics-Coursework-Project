function  [inequality_constraints, equality_constraints] = ConstraintsFunc(X_x_r, Cost_matrix, K, D)
    
    %   Initialization
    %   x matrix denotes the pipes transportated from Si to A_j.
    x = X_x_r(1:7,1:15);
    %   r(rj) denotes the length of piplines paving in the right hand side of Aj
    %   lj = Dj-1 - rj-1. 
    r = X_x_r(8,1:15);
    
    %   Caluate the sum of xij based on i(1~7) and j(1~15) respectively
    sum_xij_i = sum(x,2);
    sum_xij_j = sum(x,1);
    
    %   Construct the inequality constraint with the capacity of factory Si
    inequality_constraints = sum_xij_i - K';
    %   Construct the inequality constraints about the length of piplines paving in Aj's right hand side should
    % not larger than the distance between Aj and A(j+1) (for j = 1 ~ 14)
    inequality_constraints = [inequality_constraints; (r(1:14) - D)'];
   
    
    %   Construct the equality constraints that the length of piplines paving in Aj's left hand side
    % plus the length of paving piplines in Aj's right hand side should equal to the total length of   
    % piplines transpotated in. [denote as Aj = rj + lj = rj + Dj-1 - rj-1, where Aj is the sum of xij
    % based on j]  (for j = 2~13)
    equality_constraints = (sum_xij_j(2:14) - r(2:14) + r(1:13) - D(1:13))';
    
    %   For the A1 and A15, they only have right hand side and left hand side respectively.
    %   A1 = r1
    equality_constraints(14) = sum_xij_j(1) - r(1);
    %   r15 = 0
    equality_constraints(15) = r(15);
    %   The total length of piplines paving should equal to the distance between A1 and A15.
    equality_constraints(16) = sum(sum_xij_i) - sum(D);
    %   Assume S4 do not sell pipes
    %  equality_constraints(17) = sum_xij_i(4);
    %   Assume S7 either do not sell pipes or sell 500 pipes, not both
    %  equality_constraints(18) = sum_xij_i(7);
    %  equality_constraints(18) = sum_xij_i(7)-500;
    
    
end