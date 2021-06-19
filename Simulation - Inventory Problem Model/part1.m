%Set Initial Values
%Time:
T=30;                   %limit time
t=[];t(1)=0;t(2)=0;     %current time
L=2/24;                 %Delivery time
t0=-(1/8)*log(rand(1)); %The arrival time of one customer(initial value)
t1=t0+2/24;             %The arrival time of product(initial value)
%Quantity:
x=[];x(1)=0;x(2)=0;     %Current inventory level
D=[];D(1)=0;            %Demanding/ customer
s=10;S=120;             %Minimum & Maximum inventory level
y=0;                    %Quantity of goods ordered but not arrived
%Cost price
r=12;                   %sell price/unit
h=0.5;                  %store price/unit     
c=5;                    %cost price/unit
Dv=10;                  %Delivery cost/ride
%Cost series
H=[];H(1)=0;            %Storing cost               
C=[];C(1)=0;            %Cost price 
Loss=[];                %Money that should have been earned is not earned
R=[];R(1)=0;            %Revenue
Profit=[];Profit(1)=0;  %Net Profit
%Counters
i=2;                    %Counter of event    
d=0;                    %Counter of delivery 
Event_type=['t0'];

while t(i)<=T
    if t0<t1             %Customer arrives earlier than product
        H(i)=H(i-1)+(t0-t(i))*x(i)*h;
        t(i+1)=t0;
        Event_type=[Event_type;'t0'];
        u1=rand(1);
        if u1<0.7        %Generate demanding
            D(i)=1;
        else if u1<0.9
            D(i)=2;
            else if u1<0.98
                    D(i)=3;
                else D(i)=4;
                end 
            end 
        end
        if D(i)<x(i)     %Compare demand and current inventory
            w=D(i);
            x(i+1)=x(i)-w;
            Loss(i)=0;
        else
            w=x(i);
            Loss(i)=(D(i)-x(i))*2;
            x(i+1)=0;
        end
        
        R(i)=R(i-1)+w*r-Loss(i); 
        
        if x(i+1)<s&&y==0 %Situation that needs replenishment      
            y=S-x(i+1);
            t1=t(i+1)+L; 
            d=d+1;
        end
        C(i)=C(i-1);
        u=rand(1);
        t0=-(1/8)*log(u)+t(i+1);
        
    else %if t0>t1:        Product arrives earlier than customer
       
        H(i)=H(i-1)+(t1-t(i))*x(i-1)*h;
        t(i+1)=t1;
        Event_type=[Event_type;'t1'];
        R(i)=R(i-1);
        C(i)=C(i-1)+c*y;
        x(i+1)=x(i)+y; 
        y=0;
        t1=Inf;
        D(i)=0;
    end
    Profit(i)=R(i)-C(i)-H(i)-d*Dv;
    i=i+1;
end
%adjust the length of serieses 
D(i)=0;
R(i)=R(i-1);
Profit(i)=Profit(i-1);
Loss(i)=0;
Event_type=[Event_type;'t0'];

t=t';
R=R';
D=D';
Loss=Loss';
x=x';
Profit=Profit';

results=table(t,D,x,R,Profit,Loss,Event_type); 
results.Properties.VariableNames = {'Time','Demand','Inventory_Level','Revenue','Profit','Loss','Event_type'}

%Output:
%Net Profit:
fprintf('Net Profit = %f\n',Profit(i));
%average net profit:
fprintf('Average Net Profit = %f\n',Profit(i)/t(i-1)); 
%inventory:
subplot(2,2,1)
plot(t,x)
title 'Inventory'
%demand:
subplot(2,2,2)
plot(t,D)
title 'Demand'
%revenue:
subplot(2,2,3)
plot(t,R)
title 'Revenue'
%profit:
subplot(2,2,4)
plot(t,Profit)
title 'Profit'



