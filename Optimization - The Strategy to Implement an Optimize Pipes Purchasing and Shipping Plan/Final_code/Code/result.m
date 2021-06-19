
% Denote D - Distance between location Aj to Aj+1
% Denote K - The production capacity of each factory
% Denote P - The Price for 1km of pipes for the product of factory
 D=[104 301 750 606 194 205 201 680 480 300 220 210 420 500];
 K=[800 800 1000 2000 2000 2000 3000];
 P=[160 155 155 160 155 150 160];

Transportation_Cost_matrix = transportation_cost();
Cost_matrix = (Transportation_Cost_matrix + (repmat(P,15,1)'));

% Denote C - Cost matrix
% Cost_matrix = [357.13	350.2	320.1	254.475	193.875	174.475	162.3	174.33	224.13	260.73	271.5	280.73	301.73	309.38	327.23;
%               417	410.07	379.97	340.605	280.005	260.605	248.43	234.2	284	320.6	331.37	340.6	361.6	369.25	387.1;
%                430.65	423.72	393.62	354.255	293.655	274.255	262.08	247.85	204.05	240.65	251.42	260.65	281.65	289.3	307.15;
%                480.5	473.57	443.47	404.105	343.505	324.105	311.93	297.7	253.9	222.9	211.57	220.8	241.8	249.45	267.3;
%                466.4	459.47	429.37	390.005	329.405	310.005	297.83	283.6	239.8	208.8	186.03	206.7	227.7	235.35	253.2;
%                471.8	464.87	434.77	395.405	334.805	315.405	303.23	289	245.2	214.2	202.87	191.3	171.15	157.55	175.4;
%                498.05	491.12	461.02	421.655	361.055	341.655	329.48	315.25	271.45	240.45	229.12	217.55	197.4	183.8	162;];
 
%Initialization
x0 = zeros(8,15);
lb = zeros(8,15);      

options=optimset('Algorithm' ,'active-set');

% Main function
[X_x_r,Total_Cost]=fmincon('ObjFunc',x0,[],[],[],[],lb,[],'ConstraintsFunc', options, Cost_matrix, K, D);


x = X_x_r(1:7,:)
% x = roundn(x,-3)
x_result = int32(x) 
r = X_x_r(8,:)
Total_Cost
factories_production = sum(x,2)%factory production amount

