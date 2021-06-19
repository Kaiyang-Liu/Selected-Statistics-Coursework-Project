%Use the Floyed algorithm to find the path with lowest cost
function Matrix = Floyd(Matrix,n)
    for temp = 1:n
        for i = 1:n
            for j = 1:n
                if(Matrix(i,j) > Matrix(i,temp) + Matrix(temp,j))
                    Matrix(i,j) = Matrix(i,temp) + Matrix(temp,j);
                end
            end
        end
    end
end