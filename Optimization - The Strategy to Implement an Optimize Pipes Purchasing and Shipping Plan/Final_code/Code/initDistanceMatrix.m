% Initializing function of distance matirx
function Distance_Matrix = initDistanceMatrix(n)
    Distance_Matrix = ones(n,n);  
    for i = 1:39
        for j = 1:39
            if (i == j)
                Distance_Matrix(i,j) = 0;
            else
                Distance_Matrix(i,j) = inf;
            end
        end
    end
end




