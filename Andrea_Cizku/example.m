omega=0;
alpha=0.1;
beta=0.95;
delta=0.2;

lenX=size(x);

sigma1=zeros(lenX);
sigma2=zeros(lenX);

for t=2:lenX
    sigma1(t)=omega + alpha*x(t-1)^2 + beta*sigma1(t-1);
    sigma2(t)=omega + delta^(-1)*alpha*tanh(delta*x(t-1)^2) + beta*sigma2(t-1);
end

plot(x)
hold on
plot(sigma1,'r')
plot(sigma2,'g')
legend('data','GARCH','robust GARCH')
hold off
pause


x_T=10*rand(1,1000)-5;

response1=alpha*x_T.^2;
response2=delta^(-1)*alpha*tanh(delta*x_T.^2);

plot(x_T,response1,'.');
hold on
plot(x_T,response2,'.');







