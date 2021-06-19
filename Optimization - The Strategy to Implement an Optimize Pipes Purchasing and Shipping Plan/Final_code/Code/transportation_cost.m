function Transportation_cost_matrix = transportation_cost()
% Initialization distance matirx of highways
railways_Distance_Matrix = initDistanceMatrix(39);
% Input distances of railway
railways_Distance_Matrix(8,9) = 450;
railways_Distance_Matrix(9,10) = 8;
railways_Distance_Matrix(9,11) = 1150;
railways_Distance_Matrix(11,12) = 1100;
railways_Distance_Matrix(12,1) = 202;
railways_Distance_Matrix(1,13) = 20;
railways_Distance_Matrix(13,14) = 195;
railways_Distance_Matrix(14,15) = 306;
railways_Distance_Matrix(12,2) = 1200;
railways_Distance_Matrix(12,16) = 720;
railways_Distance_Matrix(16,3) = 690;
railways_Distance_Matrix(16,17) = 520;
railways_Distance_Matrix(17,18) = 170;
railways_Distance_Matrix(18,19) = 88;
railways_Distance_Matrix(19,5) = 462;
railways_Distance_Matrix(18,4) = 690;
railways_Distance_Matrix(18,20) = 160;
railways_Distance_Matrix(20,21) = 70;
railways_Distance_Matrix(20,22) = 320;
railways_Distance_Matrix(22,23) = 160;
railways_Distance_Matrix(23,6) = 70;
railways_Distance_Matrix(23,24) = 290;
railways_Distance_Matrix(24,7) = 30;
for j = 1:39
    for i = 1:j
        if(railways_Distance_Matrix(j,i) > railways_Distance_Matrix(i,j))
        railways_Distance_Matrix(j,i) = railways_Distance_Matrix(i,j);
        else
        railways_Distance_Matrix(i,j) = railways_Distance_Matrix(j,i);
        end
    end
end

railways_Distance_Matrix = Floyd(railways_Distance_Matrix,39);

% Obtain the cost matrix of railways
Cost_railway_Matrix = 0.065 * railways_Distance_Matrix;



% Initialization distance matirx of highways
highways_Distance_Matrix = initDistanceMatrix(39);

% Input distances of highway(transportation part)
highways_Distance_Matrix(8,25) = 85;
highways_Distance_Matrix(8,26) = 40;
highways_Distance_Matrix(10,27) = 2;
highways_Distance_Matrix(11,28) = 600;
highways_Distance_Matrix(15,29) = 10;
highways_Distance_Matrix(14,30) = 5;
highways_Distance_Matrix(13,31) = 10;
highways_Distance_Matrix(1,31) = 31;
highways_Distance_Matrix(12,32) = 12;
highways_Distance_Matrix(16,33) = 42;
highways_Distance_Matrix(17,34) = 70;
highways_Distance_Matrix(19,35) = 10;
highways_Distance_Matrix(21,36) = 10;
highways_Distance_Matrix(22,37) = 62;
highways_Distance_Matrix(6,38) = 110;
highways_Distance_Matrix(23,38) = 30;
highways_Distance_Matrix(24,39) = 20;
highways_Distance_Matrix(7,39) = 20;

% Input distances of highway(paving part)
highways_Distance_Matrix(25,26) = 104;
highways_Distance_Matrix(26,27) = 301;
highways_Distance_Matrix(27,28) = 750;
highways_Distance_Matrix(28,29) = 606;
highways_Distance_Matrix(29,30) = 194;
highways_Distance_Matrix(30,31) = 205;
highways_Distance_Matrix(31,32) = 201;
highways_Distance_Matrix(32,33) = 680;
highways_Distance_Matrix(33,34) = 480;
highways_Distance_Matrix(34,35) = 300;
highways_Distance_Matrix(35,36) = 220;
highways_Distance_Matrix(36,37) = 210;
highways_Distance_Matrix(37,38) = 420;
highways_Distance_Matrix(38,39) = 500;
for j = 1:39
    for i = 1:j
        if( highways_Distance_Matrix(j,i) > highways_Distance_Matrix(i,j))
            highways_Distance_Matrix(j,i) = highways_Distance_Matrix(i,j);
        else
            highways_Distance_Matrix(i,j) = highways_Distance_Matrix(j,i);
        end
    end
end

highways_Distance_Matrix = Floyd(highways_Distance_Matrix,39);

% Obtain the cost matrix of highways
Cost_highway_Matrix = 0.1 * highways_Distance_Matrix;

% Obtain the Total cost matrix
Total_cost_matrix = ones(39,39);
for i = 1:39
    for j = 1:39
        if(Cost_railway_Matrix(i,j) > Cost_highway_Matrix(i,j))
            Total_cost_matrix(i,j) = Cost_highway_Matrix(i,j);
        else
            Total_cost_matrix(i,j) = Cost_railway_Matrix(i,j);
        end
    end
end

Total_cost_matrix = Floyd(Total_cost_matrix,39);

Transportation_cost_matrix = ones(7,15);
for i = 1:7
    for j = 25:39
        Transportation_cost_matrix(i,j-24) = Total_cost_matrix(i,j);
    end
end

end
    

            
