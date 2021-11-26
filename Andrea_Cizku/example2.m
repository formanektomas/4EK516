omega_ini=0.1;
alpha_ini=0.2;
beta_ini=0.7;

theta_ini= [omega_ini,alpha_ini,beta_ini];

lb=[0.00000001,0,0];
ub=[100,1,1];

fce=@(theta) - llik_fun_GARCH(x,theta);

[theta_hat,llik_val]=fmincon(fce,theta_ini,[],[],[],[],lb,ub);

omega_hat=theta_hat(1)
alpha_hat=theta_hat(2)
beta_hat=theta_hat(3)
