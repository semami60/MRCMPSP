sets
i index of projects/project1*project2/
j index of activity/act1*act13/
m index of mode  /m1,m2/
c index of contractor/contractor1*contractor3/
t index of time/t1*t5/
r index of r/r1*r2/
n index of n/n1*n2/
k index of objective /1*3/;
********************
set iter /iter1*iter100/;


alias (c,cp);
$onecho > taskin3.txt

*************************************
par=yhat rng=yhat(i,j,c,m)!A1:h18 Rdim=2   Cdim=2
par=zhat rng=zhat(i,c)!A1:d3  Rdim=1   Cdim=1
par=l1 rng=l1(i,r)!A1:c3 Rdim=1   Cdim=1
par=l2 rng=l2(i,n)!A1:c3  Rdim=1   Cdim=1
par=d rng=d(i,j,c,m)!A1:h18 Rdim=2   Cdim=2
par=it rng=it(i,j)!A1:n3  Rdim=1   Cdim=1
par=subm rng=subm(i,j,c,m)!A1:h28  Rdim=2   Cdim=2
par=pr rng=pr(i,c)!A1:d3  Rdim=1   Cdim=1
par=q rng=q(i,c)!A1:d3  Rdim=1   Cdim=1
par=aa rng=aa(j,jj)!A1:n14  Rdim=1   Cdim=1
par=p rng=p(i,ii)!A1:c3 Rdim=1   Cdim=1
par=c1 rng=c1(i,j,c,m)!A1:h18  Rdim=2   Cdim=2
par=c2 rng=c2(i,c)!A1:d3 Rdim=1 Cdim=1
par=tc rng=tc(i,c)!A1:d3 Rdim=1   Cdim=1
par=pe rng=pe(i)!A1:b2 Rdim=1
par=pee rng=pee(i)!A1:b2 Rdim=1
par=ct rng=ct(i,c)!A1:d3  Rdim=1   Cdim=1

$offecho
$call gdxxrw.exe  sample3.xlsx @taskin3.txt
$gdxin  sample3.gdx
$ontext

$load i k j m c t r n
$offtext
alias(j,jj);
alias(i,ii);
alias(c,cc);
scalar b /140000/;
scalar mm /999/;

Parameters
yhat(i,j,c,m)
zhat(i,c)
l1(i,r)
l2(i,n)
d(i,j,c,m)
c1(i,j,c,m)
p(i,ii)
aa(j,jj)
q(i,c)
pr(i,c)
subm(i,j,c,m)
it(i,j)
c2(i,c)
tc(i,c)
pe(i)
pee(i)
ct(i,c)



$load  p aa subm it ct

$gdxin
;
execseed = 1+gmillisec(jnow);

l1(i,r)=uniform(35,40);
l2(i,n)=uniform(35,40);
d(i,j,c,m)=uniform(1,10);
c1(i,j,c,m)=uniform(20,60);
q(i,c)=uniform(5,9);
pr(i,c)=uniform(2000,3000);
c2(i,c)=uniform(10,15);
tc(i,c)=uniform(100,200);
pe(i)= uniform(1,2);
pee(i)= uniform(1,2);

parameter subm(i,j,c,m);
subm(i,j,c,m)=1;
parameter ct(i,c);
ct(i,c)=1;



parameter xhat(i,j,c,t);


***************

option mip=CPLEX;
parameter rec(i,j,m,r);
rec(i,j,m,r)=uniform(1,2);
parameter nrec(i,j,m,n);
nrec(i,j,m,n)= uniform(1,2);
option mip=cplex;
option optcr=0;

parameter
walfa(k) /1      .3
          2      .3
          3      .4/
wbeta(k) /1      0.4
          2      0.3
          3      0.3/;
positive variable
alfa1
beta1
alfa2
beta2
alfa3
beta3
f(i,c)
ta(i,c)
ea(i,c)
fmax
ff(i,j,c)
s(i,j,c)
s(i,jj,c)
gr(i,j,c,m)
zr(i,j,c)
tt(i,j,c)
oalfa(k)
obeta(k)

;
variable

objsub
parameter
f1max
f1min
f1menha
f2max
f2min
f2menha
f3max
f3min
f3menha
y1hat
y2hat
y3hat
;

****************************************************************************************


positive variables
of(i,c)
oea(i,c)
ota(i,c)
off(i,j,c)
os(i,j,c)
ofmax

;

binary variable
oz(i,c)
ox(i,j,c,t)
oy(i,j,c,m)
ozz(i,j,c,t,m)
oyz(i,j,c,m)
oyy(k)

;
variables
oz1 total quality
oz2 total cost
oz3 completion time
oz4
zgp
ogr(i,j,c,m)
ozr(i,j,c)


;


positive variable ogr, ozr;


scalar yr /.05/;
scalar psi/1/ ;

scalar gamma /1/;

scalar ar/1/;
parameter drh(i,j,c,m);
drh(i,j,c,m)= yr*d(i,j,c,m);

display drh;

equations
oobj1
oobj2
oobj3
ocon0(i,j,c,m)
ocon00(i,j,c,t)
ocon1(i,j)
ocon2(i)
ocon3(i,j,c,jj)
ocon03(i,j,c)
ocon0003(i,j,c,m)
ocon00003(i,j,c,m)
ocon4(i,j,c,jj)
ocon5(i,j,c)
ocon6(i,ii,c,cp,j)
ocon7(i,c)
ocon8
ocon9(i,c,r,t)
ocon10(i,c,n)
ocon11(i,c)
ocon12(i,c)
ocon13(i,c)
ocon14(c)
ocon15(i,j,c,t,m)
ocon16(i,j,c,t,m)

ocon17(i,j,c,t)
ocon18(i,j,c,t)
ocon19(i,j,c)

;
oobj1..oz1=e=-sum((i,c)$(ct(i,c)),q(i,c)*oz(i,c));
oobj2..oz2=e=sum((i,j,m,c)$(ct(i,c) and it(i,j)and subm(i,j,c,m)),oy(i,j,c,m)*c1(i,j,c,m))
 +sum((i,c)$(ct(i,c)),of(i,c)*c2(i,c)+pe(i)*ota(i,c)+pee(i)*oea(i,c));

oobj3..oz3=e=ofmax;

ocon0(i,j,c,m)$(it(i,j) and ct(i,c)and subm(i,j,c,m)) .. oy(i,j,c,m)=l=oz(i,c);
ocon1(i,j)$(it(i,j) ) ..sum((c,m)$(subm(i,j,c,m)),oy(i,j,c,m))=e=1;

ocon2(i)..sum(c$(ct(i,c)),oz(i,c))=e=1;


ocon00(i,j,c,t)$(ct(i,c)and it(i,j)) ..     ox(i,j,c,t)=l=oz(i,c) ;

ocon3(i,j,c,jj)$(it(i,j) and aa(j,jj) and ct(i,c)and (ord(j)<ord(jj))).. os(i,j,c)+sum(m $(subm(i,j,c,m)),oy(i,j,c,m)*d(i,j,c,m))+psi*sum(m$(subm(i,j,c,m)),ogr(i,j,c,m))+gamma*ozr(i,j,c)=l=os(i,jj,c);

ocon03(i,j,c)$(it(i,j) and ct(i,c))..os(i,j,c)+sum(m $(subm(i,j,c,m)),oy(i,j,c,m)*d(i,j,c,m))+psi*sum(m$(subm(i,j,c,m)),ogr(i,j,c,m))+gamma*ozr(i,j,c)-of(i,c)=l=0;


********
ocon0003(i,j,c,m)$(ct(i,c) and it(i,j))..ozr(i,j,c)+ogr(i,j,c,m)=g=drh(i,j,c,m)*oy(i,j,c,m);


ocon00003(i,j,c,m)$(ct(i,c)and it(i,j))..drh(i,j,c,m)*oy(i,j,c,m)=l=ar;
ocon4(i,j,c,jj)$(it(i,j) and aa(j,jj) and ct(i,c) and (ord(j)<ord(jj)))..off(i,j,c)=l=os(i,jj,c);
ocon5(i,j,c)$(it(i,j) and ct(i,c))..of(i,c)=g=off(i,j,c);
ocon6(i,ii,c,cp,j)$(p(i,ii) and ct(i,c)and ct(ii,cp) and it(ii,j))..of(i,c)+mm*(oz(ii,cp)-1)=l=os(ii,j,cp);

ocon7(i,c)$(ct(i,c))..sum((j,m)$(it(i,j)and subm(i,j,c,m)),oy(i,j,c,m)*c1(i,j,c,m))+of(i,c)*c2(i,c)+pe(i)*ota(i,c)+pee(i)*oea(i,c)=l=pr(i,c);

ocon8..sum((i,c)$(ct(i,c)),sum((j,m)$(it(i,j)and subm(i,j,c,m)),oy(i,j,c,m)*c1(i,j,c,m))+of(i,c)*c2(i,c)+pe(i)*ota(i,c)+pee(i)*oea(i,c))=l=b;

ocon9(i,c,r,t)$(ct(i,c))..sum((j,m)$(it(i,j)and subm(i,j,c,m)),ozz(i,j,c,t,m)*rec(i,j,m,r))=l=l1(i,r);
ocon10(i,c,n)$(ct(i,c))..sum((j,m)$(it(i,j)and subm(i,j,c,m)),oy(i,j,c,m)*nrec(i,j,m,n))=l=l2(i,n);
ocon11(i,c)$(ct(i,c))..ota(i,c)=g=of(i,c)-tc(i,c);
ocon12(i,c)$(ct(i,c))..oea(i,c)=g=tc(i,c)*oz(i,c)-of(i,c);
ocon13(i,c)$(ct(i,c)) .. ofmax=g=of(i,c);
ocon14(c) .. os('project1','act1',c)=e=0;
ocon15(i,j,c,t,m)$(ct(i,c)and it(i,j))..ox(i,j,c,t)+oy(i,j,c,m)-2*ozz(i,j,c,t,m)=g=0;
ocon16(i,j,c,t,m)$(ct(i,c)and it(i,j))..ox(i,j,c,t)+oy(i,j,c,m)-ozz(i,j,c,t,m)=l=1;

ocon17(i,j,c,t)$(ct(i,c) and it(i,j)) .. ord(t)+mm*(1-ox(i,j,c,t))=g=os(i,j,c);
ocon18(i,j,c,t)$(ct(i,c) and it(i,j))   .. ord(t)+mm*(ox(i,j,c,t)-1)=l=os(i,j,c)-1+sum((m)$(subm(i,j,c,m)),oy(i,j,c,m)*d(i,j,c,m))+psi*sum(m$(subm(i,j,c,m)),ogr(i,j,c,m))+gamma*ozr(i,j,c);
ocon19(i,j,c)$(ct(i,c) and it(i,j)).. sum(t,ox(i,j,c,t))=g=-1+sum((m)$(subm(i,j,c,m)),oy(i,j,c,m)*d(i,j,c,m))+psi*sum(m$(subm(i,j,c,m)),ogr(i,j,c,m))+gamma*ozr(i,j,c);


option mip=cplex;
option limrow=10000;

model aa2  /oobj2,ocon0,ocon00,ocon1,ocon2,ocon3,ocon03,ocon0003,ocon00003,ocon4,ocon5,ocon6,ocon7,ocon8,ocon9,ocon10,ocon11,ocon12,ocon13,ocon14,ocon15,ocon16/;
model aa3  /oobj3,ocon0,ocon00,ocon1,ocon2,ocon3,ocon03, ocon0003, ocon00003,ocon4,ocon5,ocon6,ocon7,ocon8,ocon9,ocon10,ocon11,ocon12,ocon13,ocon14,ocon15,ocon16/;



parameter ff1min(i);
ff1min(i)=0;
loop(i,
loop(c,ff1min(i)=max(ff1min(i),q(i,c))
);
);

parameter f1min;
f1min=-sum(i, ff1min(i));
display f1min;



parameter ff1max(i);
ff1max(i)=10000;
loop(i,
loop(c,ff1max(i)=min(ff1max(i),q(i,c))
);
);

parameter f1max;
f1max=-sum(i, ff1max(i))-.5;
f1menha=-sum(i, ff1max(i));
*display q,ff1min,fff1min;

****************************************
solve  aa2 using mip minimizing oz2;

f2min=oz2.l;

f2max=b-1000;
f2menha=b;
*************************

parameter
yyhat(i,j,c,m)
zzhat(i,c)
xxhat(i,j,c,t);

yyhat(i,j,c,m)=oy.l(i,j,c,m);
zzhat(i,c)=oz.l(i,c);
xxhat(i,j,c,t)=ox.l(i,j,c,t);

f1max=-sum((i,c)$(ct(i,c)),q(i,c)*oz.l(i,c))-.5;
f1menha=-sum((i,c)$(ct(i,c)),q(i,c)*oz.l(i,c));


*Initial solution

parameter totd(i,c);
parameter min_m(j);

loop(i,
loop(c,
min_m(j)=1000000;
loop(j,
loop(m,
min_m(j)=min(min_m(j),d(i,j,c,m))
);
);
totd(i,c)=sum(j, min_m(j));
);
);

display totd;
parameter min_totd(i);

loop(i,
min_totd(i)=1000000;
loop(c,
min_totd(i)=min(min_totd(i),totd(i,c));
);
);
display min_totd;

zhat(i,c)=0;
loop((i,c),
if(totd(i,c)= min_totd(i), zhat(i,c)=1);

);
display zhat;

** assign Yhat
yhat(i,j,c,m)=0;
parameter mind;
loop((i,c)$zhat(i,c),

loop((j),
mind=10000;
loop(m,
mind=min(mind,d(i,j,c,m))
);

loop(m,
if(d(i,j,c,m)=mind, yhat(i,j,c,m)=1);

);
);
);
display yhat;
*********

f3min=sum((i,j,c,m),yhat(i,j,c,m)*d(i,j,c,m));
*assign Xhat

solve  aa3 using mip maximizing oz3;
if(aa3.modelstat>=2,f3menha=4*f3min; f3max= f3menha-.1* f3min ;

else
f3max=oz3.l-2;
f3menha=oz3.l;  );
display f3min,f3max,f3menha;


yhat(i,j,c,m)=yyhat(i,j,c,m);
zhat(i,c)=zzhat(i,c);
xhat(i,j,c,t)=xxhat(i,j,c,t);


*
*********************************************************
**************************************************************
*****************************************************inja eshtebah bod

******************************************************************************************************************************

equations
object
congp1
congp2
congp3
congp4(k)
congp5(k)
congp6(k)
;

object .. zgp=e=sum(k,walfa(k)*oalfa(k)-wbeta(k)*obeta(k) );


congp1..   -sum((i,c)$(ct(i,c)),q(i,c)*oz(i,c))=e=oalfa('1')*f1min+(1- oalfa('1'))*f1max+obeta('1')*(f1menha-f1max);
congp2 ..  sum((i,c,j,m)$(ct(i,c) and it(i,j)and subm(i,j,c,m)),oy(i,j,c,m)*c1(i,j,c,m))+sum((i,c)$(ct(i,c)),of(i,c)*c2(i,c)+pe(i)*ota(i,c)+pee(i)*oea(i,c))=e=oalfa('2')*f2min+(1- oalfa('2'))*f2max+obeta('2')*(f2menha-f2max);
congp3 ..   ofmax=e=oalfa('3')*f3min+(1- oalfa('3'))*f3max+obeta('3')*(f3menha-f3max);
congp4(k)  .. oyy(k)=g=oalfa(k);
congp5(k) ..  oyy(k)=l=1+oalfa(k);
congp6(k) ..  obeta(k)+oyy(k)=l=1;



model goal_prog /object,oobj1,oobj2,oobj3,congp1,congp2,congp3,congp4,congp5,congp6,ocon0,ocon00,ocon1,ocon2,ocon3, ocon03, ocon0003, ocon00003,ocon6,ocon7,ocon8,ocon9,ocon10,ocon11,ocon12,ocon13,ocon14,ocon15,ocon16/;


goal_prog.optcr=0;

goal_prog.solprint=2;
* speed up by keeping GAMS in memory:
goal_prog.solvelink=2;
goal_prog.reslim=3600;

solve  goal_prog using mip maximizing zgp;


display zgp.l;

*The end Orginal
**********************************************************************************************************

*$ontext
y1hat=0;
y2hat=1;
y3hat=1;
************************



***************

*****************************
* main BD
****************************
*************************************
********************************************************************************
*$ontext

scalar UB ’upperbound’ /1/;
scalar LB ’lowerbound’ /0/;

scalar unbounded /1.0e6/;

equations
obj
con1
con2
con3
con4
con5
con6
con7
con8
con9
con10
con11
con12
con13
con14
con15
con16
con17
con18
con19
con20
con21
con22
con23
con24
con25
;

obj..objsub=e=(walfa('1')*alfa1-wbeta('1')*beta1)+(walfa('2')*alfa2-wbeta('2')*beta2)+(walfa('3')*alfa3-wbeta('3')*beta3);

con1..    (f1max-f1min)*alfa1+(f1max-f1menha)*beta1=e=f1max+sum((i,c)$(ct(i,c)),q(i,c)*zhat(i,c));
con2 ..   sum((i,j,m,c)$(ct(i,c) and it(i,j)and subm(i,j,c,m)),yhat(i,j,c,m)*c1(i,j,c,m)) +sum((i,c)$(ct(i,c)),f(i,c)*c2(i,c)+pe(i)*ta(i,c)+pee(i)*ea(i,c))=e=alfa2*f2min+(1- alfa2)*f2max+beta2*(f2menha-f2max);
con3..    fmax+(f3max-f3min)*alfa3+(f3max-f3menha)*beta3=e=f3max;

con4..alfa1=l=y1hat;
con5..alfa2=l=y2hat;
con6..alfa3=l=y3hat;
con7..-alfa1=l=1-y1hat-.01;
con8..-alfa2=l=1-y2hat-.01;
con9..-alfa3=l=1-y3hat-.01;
con10..beta1=l=1-y1hat;
con11..beta2=l=1-y2hat;
con12..beta3=l=1-y3hat;


con13(i,j,c,m)$(ct(i,c) and it(i,j) and subm(i,j,c,m))..drh(i,j,c,m)*yhat(i,j,c,m)-zr(i,j,c)-gr(i,j,c,m)=l=0;
con14(i,j,c,jj)$(it(i,j) and aa(j,jj) and ct(i,c)and (ord(j)<ord(jj))).. s(i,j,c)+sum(m $(subm(i,j,c,m)),yhat(i,j,c,m)*d(i,j,c,m))+psi*sum(m$(subm(i,j,c,m)),gr(i,j,c,m))+gamma*zr(i,j,c)=l=s(i,jj,c);

con15(i,j,c)$(it(i,j) and ct(i,c))..s(i,j,c)+sum(m $(subm(i,j,c,m)),yhat(i,j,c,m)*d(i,j,c,m))+psi*sum(m$(subm(i,j,c,m)),gr(i,j,c,m))+gamma*zr(i,j,c)-f(i,c)=l=0;


con16(i,ii,c,cp,j)$(p(i,ii) and ct(i,c) and ct(ii,cp)and it(ii,j)).. f(i,c)-s(ii,j,cp)=l=mm*(1-zhat(ii,cp));

con17(i,c)$(ct(i,c))..f(i,c)*c2(i,c)+pe(i)*ta(i,c)+pee(i)*ea(i,c)=l=pr(i,c)-sum((j,m)$(it(i,j)and subm(i,j,c,m)),yhat(i,j,c,m)*c1(i,j,c,m));
con18..sum((i,c)$(ct(i,c)),f(i,c)*c2(i,c))+sum((i,c)$(ct(i,c)),pe(i)*ta(i,c))+sum((i,c)$(ct(i,c)),pee(i)*ea(i,c))=l=b-sum((i,j,c,m)$(it(i,j)and subm(i,j,c,m) and ct(i,c)),yhat(i,j,c,m)*c1(i,j,c,m));

con19(i,c)$(ct(i,c))..(f(i,c)-ta(i,c))=l=tc(i,c);
con20(i,c)$(ct(i,c))..(-f(i,c)-ea(i,c))=l=-tc(i,c)*zhat(i,c);
con21(i,c)$(zhat(i,c)) .. f(i,c)-fmax=l=0;
con22(c) .. s('project1','act1',c)=e=0;

con23(i,j,c,t)$(ct(i,c) and it(i,j)) .. ord(t)+mm*(1-xhat(i,j,c,t))=g=s(i,j,c);
con24(i,j,c,t)$(ct(i,c) and it(i,j))   .. ord(t)+mm*(xhat(i,j,c,t)-1)=l=s(i,j,c)-1+sum((m)$(subm(i,j,c,m)),yhat(i,j,c,m)*d(i,j,c,m))+psi*sum(m$(subm(i,j,c,m)),gr(i,j,c,m))+gamma*zr(i,j,c);
con25(i,j,c)$(ct(i,c) and it(i,j)).. sum(t,xhat(i,j,c,t))=g=-1+sum((m)$(subm(i,j,c,m)),yhat(i,j,c,m)*d(i,j,c,m))+psi*sum(m$(subm(i,j,c,m)),gr(i,j,c,m))+gamma*zr(i,j,c);


option mip=cplex;


model subp  /obj,con1,con2,con3,con4,con5,con6,con7,con8,con9,con10,con11,con12,con13,con14,con15,con16,con17,con18,con19,con20,con21,con22/;




* reduce output to listing file:
subp.solprint=2;
* speed up by keeping GAMS in memory:
subp.solvelink=2;
*solve  subp using mip maximizing objsub;
*---------------------------------------------------------------------
* Benders Decomposition Initialization
*---------------------------------------------------------------------
*dual sub problem
variable
dobjfe
dx1
dx2
dx3
dx22(c)
positive variables
dx4
dx5
dx6
dx10
dx11
dx12
dx13
dx14
dx15
dx16
dx20
dx7
dx8
dx9
dx17
dx18
dx19
dx20
dx21
dx22
dx25
dx26
dx27
dx28
dx29
dx30
dx31(i,j,c,t)
dx32(i,j,c,t)
dx33(i,j,c)
;

parameter set_CC(i,j,c);
set_CC(i,j,c)=0;
set_CC('project1','act1',c)=1;

parameter set_BB(ii,j,c);
set_BB(ii,j,c)=0;
loop((i,ii)$(p(i,ii)),set_BB(ii,j,c)=1);
parameter set_bb1(i,j,c);
set_BB1(i,j,c)=1-set_BB(i,j,c);
display set_CC,set_BB;
equations
dobjf00
dobjf
dconf0
dconf1
dconf2
dconf3(i,c)
dconf4(i,c)
dconf5(i,c)
dconf6
dconf7
dconf8
dconf9
dconf10
dconf11(i,j,c)
dconf12(i,j,c)
dconf122
dconf13(i,j,c,m)
dconf14(i,j,c)
dconf15(i,j,c)
dconf16
;
parameter dual_value;

dobjf..dobjfe=e=
(f1max+sum((i,c)$(ct(i,c)),q(i,c)*zhat(i,c)))*dx1
+(f2max-sum((i,j,c,m)$(ct(i,c) and it(i,j)and subm(i,j,c,m)),yhat(i,j,c,m)*c1(i,j,c,m)))*dx2
+f3max*dx3
+y1hat*dx4
+y2hat*dx5
+y3hat*dx6
+(1-y1hat-.01)*dx7
+(1-y2hat-.01)*dx8
+(1-y3hat-.01)*dx9
+(1-y1hat)*dx10
+(1-y2hat)*dx11
+(1-y3hat)*dx12

-sum((i,j,c,m)$(ct(i,c)and it(i,j)and subm(i,j,c,m)),dx13(i,j,c,m)*yhat(i,j,c,m)*drh(i,j,c,m))

-sum((i,j,c,jj)$(ct(i,c)and it(i,j) and aa(j,jj) and (ord(j)<ord(jj))),dx14(i,j,c,jj)*sum(m$(subm(i,j,c,m)),yhat(i,j,c,m)*d(i,j,c,m)))

-sum((i,j,c)$(ct(i,c)and it(i,j)),dx15(i,j,c)*sum(m $(subm(i,j,c,m)),yhat(i,j,c,m)*d(i,j,c,m)))
+sum((i,ii,c,cp,j)$(p(i,ii) and ct(i,c) and ct(ii,cp) and it(ii,j)),dx16(i,ii,c,cp,j)*mm*(1-zhat(ii,cp)))
+sum((i,c)$(ct(i,c)),pr(i,c)*dx17(i,c))

-sum((i,c)$(ct(i,c)),dx17(i,c)*sum((j,m)$(subm(i,j,c,m) and it(i,j)),yhat(i,j,c,m)*c1(i,j,c,m)))

+(b-sum((i,j,c,m)$(ct(i,c)and it(i,j)and subm(i,j,c,m)),yhat(i,j,c,m)*c1(i,j,c,m)))*dx18
+sum((i,c)$(ct(i,c)),tc(i,c)*dx19(i,c))
-sum((i,c)$(ct(i,c)),tc(i,c)*dx20(i,c)*zhat(i,c))

*+sum((i,j,c,t)$ (ct(i,c)and it(i,j)), dx23(i,j,c,t)*(ord(t)+mm*(1-xhat(i,j,c,t))))
*+sum((i,j,c,t)$(ct(i,c)and it(i,j)), dx24(i,j,c,t) *(-ord(t)-mm*(xhat(i,j,c,t)-1)-1+sum((m)$(subm(i,j,c,m)),yhat(i,j,c,m)*d(i,j,c,m))))
*+sum((i,j,c)$(ct(i,c) and it(i,j)),  dx25(i,j,c) *(sum(t,xhat(i,j,c,t))+1-sum((m)$(subm(i,j,c,m)),yhat(i,j,c,m)*d(i,j,c,m))))
;


*alfa1 (ok)

dconf1.. (f1max-f1min)*dx1+dx4-dx7=g=walfa('1');

*beta1  (ok)


dconf2..(f1max-f1menha)*dx1+dx10=g=-wbeta('1');

*f(i,c)

dconf3(i,c)$(ct(i,c))..c2(i,c)*dx2-sum(j$(it(i,j)),dx15(i,j,c))+sum((ii,cp,j)$(p(i,ii)and ct(ii,cp) and it(ii,j)),dx16(i,ii,c,cp,j))+c2(i,c)*dx17(i,c)+c2(i,c)*dx18+dx19(i,c)-dx20(i,c)+dx21(i,c)*zhat(i,c)=g=0;


*Ta(i,c)

dconf4(i,c)$(ct(i,c))..pe(i)*dx2+pe(i)*dx17(i,c)+pe(i)*dx18-dx19(i,c)=g=0;


*Ea(i,c)

dconf5(i,c)$(ct(i,c))..pee(i)*dx2+pee(i)*dx17(i,c)+pee(i)*dx18-dx20(i,c)=g=0;


*alfa2

dconf6   ..(f2max-f2min)*dx2+dx5-dx8=g=walfa('2');


*beta2


dconf7    ..(f2max-f2menha)*dx2+dx11=g=-wbeta('2');

*fmax


dconf8..dx3-sum((i,c)$(ct(i,c)),zhat(i,c)*dx21(i,c))=g=0;
*alfa3


dconf9..(f3max-f3min)*dx3+dx6-dx9=g=walfa('3');

*beta3

dconf10..(f3max-f3menha)*dx3+dx12=g=-wbeta('3');

*s(i,j,c)

dconf12(i,j,c)$(it(i,j) and ct(i,c) and set_bb1(i,j,c) )..sum(jj$( aa(j,jj) and (ord(jj)>ord(j))),dx14(i,j,c,jj))-sum(jj$(aa(jj,j) and (ord(jj)<ord(j))),dx14(i,jj,c,j))+dx15(i,j,c)+set_CC(i,j,c)*dx22(c)=g=0;
dconf122(ii,j,cp)$(it(ii,j) and ct(ii,cp) and set_bb(ii,j,cp) )..sum(jj$( aa(j,jj) and (ord(jj)>ord(j))),dx14(ii,j,cp,jj))-sum(jj$(aa(jj,j) and (ord(jj)<ord(j))),dx14(ii,jj,cp,j))+dx15(ii,j,cp)-sum((i,c)$(p(i,ii)and ct(i,c)),dx16(i,ii,c,cp,j))+set_CC(ii,j,cp)*dx22(cp)=g=0;

*gr(i,j,c,m)

dconf13(i,j,c,m)$(it(i,j) and ct(i,c) and subm(i,j,c,m))..psi*(sum(jj$( aa(j,jj) and (ord(jj)>ord(j))),dx14(i,j,c,jj)))-dx13(i,j,c,m)+psi*dx15(i,j,c)=g=0;

*zr(i,j,c)

dconf14(i,j,c)$(it(i,j) and ct(i,c) )..gamma*(sum(jj$( aa(j,jj) and (ord(jj)>ord(j))),dx14(i,j,c,jj)))-sum(m$(subm(i,j,c,m)),dx13(i,j,c,m))+gamma*dx15(i,j,c)=g=0;



option lp=cplex;
option limrow=10000;


option optcr=0;

model dual /dobjf,dconf1,dconf2,dconf3,dconf4,dconf5,dconf6,dconf8,dconf9,dconf10,dconf12,dconf122,dconf13,dconf14/;


****************************************************
*Modified subproblem
*******************************************

*feasible cut
variable
objfe
x1
x2
x3
x22(c)
positive variables
x4
x5
x6
x10
x11
x12
x13
x14
x16
x19
x20
x7
x8
x9
x15
x17
x18
x21
x22
x23
x25
x26
x27
x28
x29
x30
x31(i,j,c,t)
x32(i,j,c,t)
x33(i,j,c)
;

parameter set_CC(i,j,c);
set_CC(i,j,c)=0;
set_CC('project1','act1',c)=1;

equations
mobjf00
mobjf
mconf0
mconf1
mconf2
mconf3(i,c)
mconf4(i,c)
mconf5(i,c)
mconf6
mconf7
mconf8
mconf9
mconf10
mconf11(i,j,c)
mconf12(i,j,c)
mconf122
mconf13(i,j,c,m)
mconf14(i,j,c)
mconf15(i,j,c)
mconf16
;


mobjf..objfe=e=
(f1max+sum((i,c)$(ct(i,c)),q(i,c)*zhat(i,c)))*x1
+(f2max-sum((i,j,c,m)$(ct(i,c) and it(i,j)and subm(i,j,c,m)),yhat(i,j,c,m)*c1(i,j,c,m)))*x2
+f3max*x3
+y1hat*x4
+y2hat*x5
+y3hat*x6
+(1-y1hat-.01)*x7
+(1-y2hat-.01)*x8
+(1-y3hat-.01)*x9
+(1-y1hat)*x10
+(1-y2hat)*x11
+(1-y3hat)*x12

-sum((i,j,c,m)$(ct(i,c)and it(i,j)and subm(i,j,c,m)),x13(i,j,c,m)*yhat(i,j,c,m)*drh(i,j,c,m))

-sum((i,j,c,jj)$(ct(i,c)and it(i,j) and aa(j,jj) and (ord(j)<ord(jj))),x14(i,j,c,jj)*sum(m$(subm(i,j,c,m)),yhat(i,j,c,m)*d(i,j,c,m)))

-sum((i,j,c)$(ct(i,c)and it(i,j)),x15(i,j,c)*sum(m $(subm(i,j,c,m)),yhat(i,j,c,m)*d(i,j,c,m)))

+sum((i,ii,c,cp,j)$(p(i,ii) and ct(i,c) and ct(ii,cp) and it(ii,j)),x16(i,ii,c,cp,j)*mm*(1-zhat(ii,cp)))

+sum((i,c)$(ct(i,c)),pr(i,c)*x17(i,c))

-sum((i,c)$(ct(i,c)),x17(i,c)*sum((j,m)$(subm(i,j,c,m) and it(i,j)),yhat(i,j,c,m)*c1(i,j,c,m)))

+(b-sum((i,j,c,m)$(ct(i,c)and it(i,j)and subm(i,j,c,m)),yhat(i,j,c,m)*c1(i,j,c,m)))*x18
+sum((i,c)$(ct(i,c)),tc(i,c)*x19(i,c))
-sum((i,c)$(ct(i,c)),tc(i,c)*x20(i,c)*zhat(i,c))

*+sum((i,j,c,t)$ (ct(i,c)and it(i,j)), dx29(i,j,c,t)*(ord(t)+mm*(1-xhat(i,j,c,t))))
*+sum((i,j,c,t)$(ct(i,c)and it(i,j)), dx30(i,j,c,t) *(-ord(t)-mm*(xhat(i,j,c,t)-1)-1+sum((m)$(subm(i,j,c,m)),yhat(i,j,c,m)*d(i,j,c,m))))
*+sum((i,j,c)$(ct(i,c) and it(i,j)),  dx31(i,j,c) *(sum(t,xhat(i,j,c,t))+1-sum((m)$(subm(i,j,c,m)),yhat(i,j,c,m)*d(i,j,c,m))))
;

*alfa1 (ok)

mconf1.. (f1max-f1min)*x1+x4-x7=g=0;

*beta1  (ok)


mconf2..(f1max-f1menha)*x1+x10=g=0;

*f(i,c)

mconf3(i,c)$(ct(i,c))..c2(i,c)*x2-sum(j$(it(i,j)),x15(i,j,c))+sum((ii,cp,j)$(p(i,ii)and ct(ii,cp) and it(ii,j)),x16(i,ii,c,cp,j))+c2(i,c)*x17(i,c)+c2(i,c)*x18+x19(i,c)-x20(i,c)+x21(i,c)*zhat(i,c)=g=0;

*Ta(i,c)

mconf4(i,c)$(ct(i,c))..pe(i)*x2+pe(i)*x17(i,c)+pe(i)*x18-x19(i,c)=g=0;


*Ea(i,c)

mconf5(i,c)$(ct(i,c))..pee(i)*x2+pee(i)*x17(i,c)+pee(i)*x18-x20(i,c)=g=0;


*alfa2


mconf6   ..(f2max-f2min)*x2+x5-x8=g=0;


*beta2


mconf7    ..(f2max-f2menha)*x2+x11=g=0;


*fmax

*dconf8..dx3-sum((i,c)$(ct(i,c)),zhat(i,c)*dx21(i,c))=g=0;
mconf8.. x3-sum((i,c)$(ct(i,c)),zhat(i,c)*x21(i,c))=g=0;
*alfa3

*dconf9..(f3max-f3min)*dx3+dx6-dx9+dx27=g=walfa('3');
mconf9..(f3max-f3min)*x3+x6-x9=g=0;
*dconf9..-dx9=g=walfa('3');

*beta3

*dconf10..(f3max-f3menha)*dx3+dx12+dx28=g=-wbeta('3');
mconf10..(f3max-f3menha)*x3+x12=g=0;
*dconf10..dx12=g=-wbeta('3');

*s(i,j,c)

mconf12(i,j,c)$(it(i,j) and ct(i,c) and set_bb1(i,j,c) )..sum(jj$( aa(j,jj) and (ord(jj)>ord(j))),x14(i,j,c,jj))-sum(jj$(aa(jj,j) and (ord(jj)<ord(j))),x14(i,jj,c,j))+x15(i,j,c)+set_CC(i,j,c)*x22(c)=g=0;
mconf122(ii,j,cp)$(it(ii,j) and ct(ii,cp) and set_bb(ii,j,cp) )..sum(jj$( aa(j,jj) and (ord(jj)>ord(j))),x14(ii,j,cp,jj))-sum(jj$(aa(jj,j) and (ord(jj)<ord(j))),x14(ii,jj,cp,j))+x15(ii,j,cp)-sum((i,c)$(p(i,ii)and ct(i,c)),x16(i,ii,c,cp,j))+set_CC(ii,j,cp)*x22(cp)=g=0;


*gr(i,j,c,m)


mconf13(i,j,c,m)$(it(i,j) and ct(i,c) and subm(i,j,c,m))..psi*(sum(jj$( aa(j,jj) and (ord(jj)>ord(j))),x14(i,j,c,jj)))-x13(i,j,c,m)+psi*x15(i,j,c)=g=0;

*zr(i,j,c)

mconf14(i,j,c)$(it(i,j) and ct(i,c) )..gamma*(sum(jj$( aa(j,jj) and (ord(jj)>ord(j))),x14(i,j,c,jj)))-sum(m$(subm(i,j,c,m)),x13(i,j,c,m))+gamma*x15(i,j,c)=g=0;


mconf16 ..    x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+sum((i,j,c,m)$(ct(i,c) and it(i,j) and subm(i,j,c,m)),x13(i,j,c,m))
+sum((i,j,c,jj)$(it(i,j) and aa(j,jj) and ct(i,c)and (ord(j)<ord(jj))),x14(i,j,c,jj))
+sum( (i,j,c)$(it(i,j) and ct(i,c)),x15(i,j,c))
+sum((i,ii,c,cp,j)$(p(i,ii) and ct(i,c) and ct(ii,cp) and it(ii,j)),x16(i,ii,c,cp,j))
+sum((i,c)$(ct(i,c)),x17(i,c))+x18+sum((i,c)$(ct(i,c)),x19(i,c))+sum((i,c)$(ct(i,c)),x20(i,c))
+sum((i,c)$(ct(i,c)),x21(i,c))+sum(c,x22(c))
*+sum((i,j,c,t)$(ct(i,c) and it(i,j)),x31(i,j,c,t))+sum((i,j,c,t)$(ct(i,c) and it(i,j)),x32(i,j,c,t))+sum((i,j,c)$(ct(i,c) and it(i,j)),x33(i,j,c))
=e=1;



option mip=cplex;
option limrow=10000;

*option optcr=0;
model fcut /mobjf,mconf1,mconf2,mconf3,mconf4,mconf5,mconf6,mconf7,mconf8,mconf9,mconf10,mconf12,mconf122,mconf13,mconf14,mconf16/;
* reduce output to listing file:
fcut.solprint=2;
* speed up by keeping GAMS in memory:
fcut.solvelink=2;

*****************************



**************************


*---------------------------------------------------------------------
* Benders Relaxed Master Problem
*---------------------------------------------------------------------


set cutset(iter) ’dynamic set’;
cutset(iter)=no;
set unbcutset(iter) ’dynamic set’;
unbcutset(iter)=no;
variable zmaster ,zzz(i);
positive variable mgr(i,j,c,m),mzr(i,j,c);
binary variable
y(i,j,c,m)
z(i,c)
x(i,j,c,t)
zz(i,j,c,t,m)
y1
y2
y3;
parameter result(iter,*);


set optcut(iter)  'andis pooya baraye boresh behinegi'
set unbcut(iter)  'andis pooya baraye  boresh shodani'
;
optcut(iter)=no;
unbcut(iter)=no;

parameters
ocoef_fmax(iter)
ocoef_x1(iter)
ocoef_x2(iter)
ocoef_x3(iter)
ocoef_x4(iter)
ocoef_x5(iter)
ocoef_x6(iter)
ocoef_x7(iter)
ocoef_x8(iter)
ocoef_x9(iter)
ocoef_x10(iter)
ocoef_x11(iter)
ocoef_x12(iter)
ocoef_x13(iter,i,j,c,m)
ocoef_x14(iter,i,j,c,jj)
ocoef_x15(iter,i,j,c)
ocoef_x16(iter,i,ii,c,cp,j)
ocoef_x17(iter,i,c)
ocoef_x18(iter)
ocoef_x19(iter,i,c)
ocoef_x20(iter,i,c)
ocoef_x21(iter,i,c)
ocoef_x22(iter,c)
*ocoef_x25(iter)
*ocoef_x26(iter)
*ocoef_x27(iter)
*ocoef_x28(iter)
*ocoef_x29(iter)
*ocoef_x30(iter)
ocoef_x31(iter,i,j,c,t)
ocoef_x32(iter,i,j,c,t)
ocoef_x33(iter,i,j,c)
oyhat(iter,i,j,c,m)
oxhat(iter,i,j,c,t)
ozhat(iter,i,c)

emcoef_fmax(iter)
emcoef_x1(iter)
emcoef_x2(iter)
emcoef_x3(iter)
emcoef_x4(iter)
emcoef_x5(iter)
emcoef_x6(iter)
emcoef_x7(iter)
emcoef_x8(iter)
emcoef_x9(iter)
emcoef_x10(iter)
emcoef_x11(iter)
emcoef_x12(iter)
emcoef_x13(iter,i,j,c,m)
emcoef_x14(iter,i,j,c,jj)
emcoef_x15(iter,i,j,c)
emcoef_x16(iter,i,ii,c,cp,j)
emcoef_x17(iter,i,c)
emcoef_x18(iter)
emcoef_x19(iter,i,c)
emcoef_x20(iter,i,c)
emcoef_x21(iter,i,c)
emcoef_x22(iter,c)
*emcoef_x25(iter)
*emcoef_x26(iter)
*emcoef_x27(iter)
*emcoef_x28(iter)
*emcoef_x29(iter)
*emcoef_x30(iter)
emcoef_x31(iter,i,j,c,t)
emcoef_x32(iter,i,j,c,t)
emcoef_x33(iter,i,j,c)
emyhat(iter,i,j,c,m)
emxhat(iter,i,j,c,t)
emzhat(iter,i,c);

equations
cut(iter) ’Benders cut for optimal subproblem’
unboundedcut1(iter)
cut1(iter)
cut2
unboundedcut(iter) ’Benders cut for unbounded subproblem’
mcon0
mcon1
mcon2
mcon3
mcon4
mcon5
mcon6
mcon7
mcon8
mcon9
mcon10
*mcon11
mcon12
mcon13
mcon14
mcon15
mcon16
mcon17
mcon18
optimalitycut(iter)  'benders optimality cut'
feasibilitycut(iter) ’Benders cut for unbounded subproblem’
;

feasibilitycut(unbcut) ..(f1max+sum((i,c)$(ct(i,c)),q(i,c)*z(i,c)))*emcoef_x1(unbcut)
+(f2max-sum((i,j,c,m)$(ct(i,c) and it(i,j)and subm(i,j,c,m)),y(i,j,c,m)*c1(i,j,c,m)))*emcoef_x2(unbcut)
+f3max*emcoef_x3(unbcut)
+y1*emcoef_x4(unbcut)+y2*emcoef_x5(unbcut)+y3*emcoef_x6(unbcut)
+(1-y1-.01)*emcoef_x7(unbcut)+(1-y2-.01)*emcoef_x8(unbcut)+(1-y3-.01)*emcoef_x9(unbcut)+(1-y1)*emcoef_x10(unbcut)+(1-y2)*emcoef_x11(unbcut)+(1-y3)*emcoef_x12(unbcut)
-sum((i,j,c,m)$(ct(i,c)and it(i,j)and subm(i,j,c,m)),emcoef_x13(unbcut,i,j,c,m)*y(i,j,c,m)*drh(i,j,c,m))
-sum((i,j,c,jj)$(ct(i,c)and it(i,j) and aa(j,jj) and (ord(jj)>ord(j))),emcoef_x14(unbcut,i,j,c,jj)*sum(m$(subm(i,j,c,m)),y(i,j,c,m)*d(i,j,c,m)))
-sum((i,j,c)$(ct(i,c)and it(i,j)),emcoef_x15(unbcut,i,j,c)*sum(m $(subm(i,j,c,m)),y(i,j,c,m)*d(i,j,c,m)))
+sum((i,ii,c,cp,j)$(p(i,ii) and ct(i,c)and ct(ii,cp) and it(ii,j)),emcoef_x16(unbcut,i,ii,c,cp,j)*mm*(1-z(ii,cp)))
+sum((i,c)$(ct(i,c)),pr(i,c)*emcoef_x17(unbcut,i,c))
-sum((i,c)$(ct(i,c)),emcoef_x17(unbcut,i,c)*sum((j,m)$(subm(i,j,c,m) and it(i,j)),y(i,j,c,m)*c1(i,j,c,m)))
+(b-sum((i,j,c,m)$(ct(i,c)and it(i,j)and subm(i,j,c,m)),y(i,j,c,m)*c1(i,j,c,m)))*emcoef_x18(unbcut)
+sum((i,c)$(ct(i,c)),tc(i,c)*emcoef_x19(unbcut,i,c))-sum((i,c)$(ct(i,c)),tc(i,c)*z(i,c)*emcoef_x20(unbcut,i,c))

*+sum((i,j,c,t)$(ct(i,c)and it(i,j)),emcoef_x31(unbcut,i,j,c,t) *(ord(t)+mm*(1-x(i,j,c,t))))
*+sum((i,j,c,t)$(ct(i,c)and it(i,j)),emcoef_x32(unbcut,i,j,c,t) *(-ord(t)-mm*(x(i,j,c,t)-1)-1+sum(m$(subm(i,j,c,m)),y(i,j,c,m)*d(i,j,c,m))))
*+sum((i,j,c)$(ct(i,c)and it(i,j)),emcoef_x33(unbcut,i,j,c)  *(sum(t,x(i,j,c,t))+1-sum(m$(subm(i,j,c,m)),y(i,j,c,m)*d(i,j,c,m))))
=g=0;


optimalitycut(optcut).. zmaster =l= (f1max+sum((i,c)$(ct(i,c)),q(i,c)*z(i,c)))*ocoef_x1(optcut)
+(f2max-sum((i,j,c,m)$(ct(i,c) and it(i,j)and subm(i,j,c,m)),y(i,j,c,m)*c1(i,j,c,m)))*ocoef_x2(optcut)
+f3max*ocoef_x3(optcut)
+y1*ocoef_x4(optcut)+y2*ocoef_x5(optcut)+y3*ocoef_x6(optcut)
+(1-y1-.01)*ocoef_x7(optcut)+(1-y2-.01)*ocoef_x8(optcut)+(1-y3-.01)*ocoef_x9(optcut)+(1-y1)*ocoef_x10(optcut)+(1-y2)*ocoef_x11(optcut)+(1-y3)*ocoef_x12(optcut)
-sum((i,j,c,m)$(ct(i,c)and it(i,j)and subm(i,j,c,m)),ocoef_x13(optcut,i,j,c,m)*y(i,j,c,m)*drh(i,j,c,m))
-sum((i,j,c,jj)$(ct(i,c)and it(i,j) and aa(j,jj) and (ord(jj)>ord(j))),ocoef_x14(optcut,i,j,c,jj)*sum(m$(subm(i,j,c,m)),y(i,j,c,m)*d(i,j,c,m)))
-sum((i,j,c)$(ct(i,c)and it(i,j)),ocoef_x15(optcut,i,j,c)*sum(m $(subm(i,j,c,m)),y(i,j,c,m)*d(i,j,c,m)))
+sum((i,ii,c,cp,j)$(p(i,ii) and ct(i,c) and ct(ii,cp)and it(ii,j)),ocoef_x16(optcut,i,ii,c,cp,j)*mm*(1-z(ii,cp)))
+sum((i,c)$(ct(i,c)),pr(i,c)*ocoef_x17(optcut,i,c))
-sum((i,c)$(ct(i,c)),ocoef_x17(optcut,i,c)*sum((j,m)$(subm(i,j,c,m) and it(i,j)),y(i,j,c,m)*c1(i,j,c,m)))
+(b-sum((i,j,c,m)$(ct(i,c)and it(i,j)and subm(i,j,c,m)),y(i,j,c,m)*c1(i,j,c,m)))*ocoef_x18(optcut)
+sum((i,c)$(ct(i,c)),tc(i,c)*ocoef_x19(optcut,i,c))-sum((i,c)$(ct(i,c)),tc(i,c)*z(i,c)*ocoef_x20(optcut,i,c))

*+sum((i,j,c,t)$(ct(i,c)and it(i,j)),ocoef_x31(optcut,i,j,c,t) *(ord(t)+mm*(1-x(i,j,c,t))))
*+sum((i,j,c,t)$(ct(i,c)and it(i,j)),ocoef_x32(optcut,i,j,c,t) *(-ord(t)-mm*(x(i,j,c,t)-1)-1+sum(m$(subm(i,j,c,m)),y(i,j,c,m)*d(i,j,c,m))))
*+sum((i,j,c)$(ct(i,c)and it(i,j)),ocoef_x33(optcut,i,j,c) *(sum(t,x(i,j,c,t))+1-sum(m$(subm(i,j,c,m)),y(i,j,c,m)*d(i,j,c,m))))
;


********************************


*****************************************************************************************************************************
mcon0(i,j,c,m)$(it(i,j) and ct(i,c)and subm(i,j,c,m)) .. y(i,j,c,m)=l=z(i,c);
mcon1(i,j)$(it(i,j)  ) ..sum((c,m)$(subm(i,j,c,m)),y(i,j,c,m))=e=1;

mcon2(i)..sum(c$(ct(i,c)),z(i,c))=e=1;

mcon3(i,j,c,t)$(ct(i,c)) ..     x(i,j,c,t)=l=z(i,c) ;
mcon4(i,j,c,m)$(ct(i,c))..drh(i,j,c,m)*y(i,j,c,m)=l=ar;
mcon5(i,c,r,t)$(ct(i,c))..sum((j,m)$(it(i,j)and subm(i,j,c,m)),zz(i,j,c,t,m)*rec(i,j,m,r))=l=l1(i,r);
mcon6(i,c,n)$(ct(i,c))..sum((j,m)$(it(i,j)and subm(i,j,c,m)),y(i,j,c,m)*nrec(i,j,m,n))=l=l2(i,n);
mcon7(i,j,c,t,m)$(ct(i,c))..x(i,j,c,t)+y(i,j,c,m)-2*zz(i,j,c,t,m)=g=0;
mcon8(i,j,c,t,m)$(ct(i,c))..x(i,j,c,t)+y(i,j,c,m)-zz(i,j,c,t,m)=l=1;

mcon9(i,j,t)..sum((c,m)$(subm(i,j,c,m)),y(i,j,c,m)*d(i,j,c,m))=l=mm*sum(c $ (ct(i,c)),x(i,j,c,t));
mcon10(i,j,c,t)..x(i,j,c,t)=l=sum(m $(subm(i,j,c,m)),y(i,j,c,m));

mcon12(ii).. sum(i$p(i,ii),zzz(i))+sum((j,c,m)$( ct(ii,c) and it(ii,j) and subm(ii,j,c,m)),y(ii,j,c,m)*d(ii,j,c,m))=l=f3max;
mcon13(i).. zzz(i)=e=sum((j,c,m)$( ct(i,c) and it(i,j) and subm(i,j,c,m)),y(i,j,c,m)*d(i,j,c,m));

model master /feasibilitycut,optimalitycut,mcon0,mcon1,mcon2,mcon3,mcon4,mcon6,mcon7,mcon8,mcon12,mcon13/;



option optCr = 0;
option mip = cplex;


* reduce output to listing file:
master.solprint=2;
* speed up by keeping GAMS in memory:
master.solvelink=2;




parameter UB1;
parameter results(iter,*);

parameter nonconverged /yes/;
*---------------------------------------------------------------------
* Benders Algorithm
*---------------------------------------------------------------------
loop(iter,
*
* solve Benders subproblem
*
solve subp maximizing objsub using mip;



solve dual minimizing dobjfe using mip;
*
* check results.
*
*
*
* was subproblem unbounded?
*
if (objsub.l+1 < unbounded and subp.modelstat<2,
*
* no, so update upperbound
*
LB = max(LB,  objsub.l);

result(iter,'LB')=LB;
*
* and add Benders’ cut to Relaxed Master
*
optcut(iter) = yes;
else

LB=LB;

solve  fcut using lp minimizing objfe;

*
**********************************************************
*solve modifiedsubproblem maximizing dummy using lp;
**********************************************************
unbcut(iter) = yes;
);

result(iter,'LB')=LB*10000;
*

ocoef_x1(iter)=dx1.l;
ocoef_x2(iter)=dx2.l;
ocoef_x3(iter)=dx3.l;
ocoef_x4(iter)=dx4.l;
ocoef_x5(iter)=dx5.l;
ocoef_x6(iter)=dx6.l;
ocoef_x7(iter)=dx7.l;
ocoef_x8(iter)=dx8.l;
ocoef_x9(iter)=dx9.l;
ocoef_x10(iter)=dx10.l;
ocoef_x11(iter)=dx11.l ;
ocoef_x12(iter)=dx12.l  ;
ocoef_x13(iter,i,j,c,m)=dx13.l(i,j,c,m) ;
ocoef_x14(iter,i,j,c,jj)=dx14.l(i,j,c,jj);
ocoef_x15(iter,i,j,c)=dx15.l(i,j,c);
ocoef_x16(iter,i,ii,c,cp,j)=dx16.l(i,ii,c,cp,j);
ocoef_x17(iter,i,c)=dx17.l(i,c);
ocoef_x18(iter)=dx18.l;
ocoef_x19(iter,i,c)=dx19.l(i,c) ;
ocoef_x20(iter,i,c)=dx20.l(i,c);
ocoef_x21(iter,i,c)=dx21.l(i,c);
ocoef_x22(iter,c)=dx22.l(c) ;



emcoef_x1(iter)=x1.l;
emcoef_x2(iter)=x2.l;
emcoef_x3(iter)=x3.l;
emcoef_x4(iter)=x4.l;
emcoef_x5(iter)=x5.l;
emcoef_x6(iter)=x6.l;
emcoef_x7(iter)=x7.l;
emcoef_x8(iter)=x8.l;
emcoef_x9(iter)=x9.l;
emcoef_x10(iter)=x10.l;
emcoef_x11(iter)=x11.l;
emcoef_x12(iter)=x12.l;
emcoef_x13(iter,i,j,c,m)=x13.l(i,j,c,m);
emcoef_x14(iter,i,j,c,jj)=x14.l(i,j,c,jj);
emcoef_x15(iter,i,j,c)=x15.l(i,j,c);
emcoef_x16(iter,i,ii,c,cp,j) =x16.l(i,ii,c,cp,j);
emcoef_x17(iter,i,c)=x17.l(i,c);
emcoef_x18(iter)=x18.l;
emcoef_x19(iter,i,c) =x19.l(i,c);
emcoef_x20(iter,i,c)=x20.l(i,c);
emcoef_x21(iter,i,c)=x21.l(i,c);
emcoef_x22(iter,c)=x22.l(c);



option optcr=0;
solve master maximizing zmaster using mip;

yhat(i,j,c,m)=y.l(i,j,c,m);
xhat(i,j,c,t)=x.l(i,j,c,t);
y1hat=y1.l;
y2hat=y2.l;
y3hat=y3.l;
zhat(i,c)=z.l(i,c);


UB = zmaster.l;

result(iter,'UB')=UB*10000;
display UB,LB,zgp.l,result,y.l,oy.l,z.l,oz.l,x.l;
abort$( (UB-LB) < 0.0000001 ) "Converged";

);
