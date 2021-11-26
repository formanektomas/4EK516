function val=llik_fun_GARCH(x,theta)

omega=theta(1);
alpha=theta(2);
beta=theta(3);

T=length(x);

sig2=zeros(T,1);

sig2(1)=var(x);

for t=1:T-1
    sig2(t+1)=omega+alpha*x(t)^2+beta*sig2(t);
end

l=-(1/2)*log(2*pi) - (1/2)*log(sig2) - (1/2)*(x.^2)./sig2;

val=sum(l);