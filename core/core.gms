$title   Recursive-Dynamic Model for EMF 34 Trilateral Study

*	Mei Yuan
*	yuanmei@mit.edu

*	10/09/2017	Add sets definition, model parameters


*	Data Base:

$setglobal ds		%ds%

*	Time Dimension:

$setglobal bmkyr	2011
$setglobal firstyr	2015
$setglobal tint		%tint%

*	The number of model periods includes benchmark year
*	Run to a year on or before 2050 otherwise need to modify maptaa
*	to extrapolate to year beyond 2050

$setglobal nper		9

$eval tlast %firstyr%+%tint%*(%nper%-2)
$setglobal tlast	%tlast%


set	allt		Annual periods	/ %bmkyr%*2100 /;

scalar	nper		Number of model years	/ %nper% / ;

$onecho > time.gms
set	tan		Annual periods	/ %bmkyr%*%tlast% /
	nperct		Period counter	/ 2*%nper% /;

set	modelyr		Model years;
modelyr("%bmkyr%") = yes;

loop(nperct,
modelyr(tan)$(tan.val = %firstyr%+%tint%*(nperct.val-2)) = yes;	
);

file tout / time.dat /;
put tout; 
put "set  t  /" /;
put loop(modelyr, put modelyr.tl/;); put "/;" //;
put "set  tan  /" /;
put loop(tan, put tan.tl/;); put "/;" //;
putclose;
$offecho

$call "gams time --bmkyr=%bmkyr% --firstyr=%firstyr% --tint=%tint% --nper=%nper% --tlast=%tlast%";

$include time.dat	
$call "del time.* /q";

display t, tan;

parameter 
	yr(t)		Numerical year value
	lp(t)		Years to previous time period
	lpn(t)		Years to next time period;

yr(t) = t.val;

lp(t)$(ord(t) ge 2) = yr(t) - yr(t-1);
lpn(t)$(ord(t) ne card(t)) = yr(t+1) - yr(t); 
display yr, lp, lpn;

set	tt(t)		Time periods which have been solved
	tb(t)		Base period
	tlast(t)	Last period	/ %tlast% /,
	maptant(tan,t)	Mapping from annual periods to model years;

tb(t) = yes$(ord(t) eq 1);

alias (t,tal) , (tan,tann);

loop(t,
loop(tan$((tan.val ge t.val) and (tan.val lt (t.val+lpn(t)))),
maptant(tan,t) = yes;
););

maptant(tan,t)$(tlast(t) and (tan.val ge t.val)) = yes;
display maptant;

*-------------------------------------------------------------------------------
*	Conversion Factors
*-------------------------------------------------------------------------------

scalar
	valscale	Economic value scaler				/ 1 /
	cvt06to07	Currency conversion from 2006 to 2007		/ 1.027 /
	cvt06to09	Currency conversion from 2006 to 2009		/ 1.063 /
	cvt06to11	Currency conversion from 2006 to 2011		/ 1.098 /
	cvt06to13	Currency conversion from 2006 to 2013		/ 1.134 /
	cvt06to15	Currency conversion from 2006 to 2015		/ 1.169 /
	cvt06to16	Currency conversion from 2006 to 2016		/ 1.1856 /

	quad2ej		Conversion from Quad to EJ	/ 1.055 /
	quad2twh	Conversion from Quad to TWh	/ 293 /
	quad2mtoe	Conversion from Quad to mtoe	/ 25.2 /
	quad2mbpd	Conversion from Quad to million barrels per day
	quad2tcf	Conversion from Quad to trillion cubic feet of gas / 0.974 /
	quad2mtce	Conversion from Quad to million short ton of coal
;

quad2mbpd = 1000/5.8/365;

* http://www.onlineconversion.com/energy.htm  : 1 quad = 35 999 396.061 tonne of coal equivalent
* Apply EIA conversion factor given that AEO has Total coal production of 23.79 in Table A1 
* and 1163 Million Short Tons in Table A15

quad2mtce = 1/0.019622;


*-------------------------------------------------------------------------------
*	Import Data
*-------------------------------------------------------------------------------

$gdxin ..\data\%ds%

set	
	i_		Goods
	et_		Electricity generation by technology
	r		Regions
	f		Factors;

$load i_=i et_=et r f

set	
	g		All goods incl cgi		/ set.i_, ele, c, g, i /
	i(g)		Goods				/ set.i_, ele /	
	e(g)		Energy goods			/ col, gas, cru, oil, ele, set.et_ /
	ele(g)		Electricity			/ set.et_, ele /
	et(g)		Generation technologies		/ set.et_ /
	nuc(g)		Nuclear generation		/ enuc /
	hyd(g)		Hydro generation		/ ehyd /
	
	enoe(g)		Non-electricity energy		/ col, gas, cru, oil /
	xe(g)		Exhaustible goods		/ col, gas, cru /
	fe(g)		Fossil energy			/ col, gas, oil /
	col(g)		Coal				/ col /
	gas(g)		Natural gas			/ gas /
	cru(g)		Crude oil			/ cru /
	oil(g)		Refined Petroleum products	/ oil /
	agr(g)		Agriculture goods		/ agr /
	eis(g)		Energy intensive industries	/ eis /
	srv(g)		Services			/ srv /
	trn(g)		Commercial transportation	/ trn /
	oth(g)		Other industries		/ oth /
	mvh(g)		Motor vehicle equipment		/ mvh /
	tnd(g)		Transmission and distribution	/ tnd /
	fd(g)		Final demand			/ c, g, i /
	hhtrn		Household transportation	/ hhtrn /	
	ag		All sectors for reporting	/ set.g, set.hhtrn /;

alias (r,s), (i,j), (g,gg), (f,ff);

display ag;

set	ne(g)		Non-energy goods
	sf(f)		Sluggish primary factors (sector-specific)
	mf(f)		Mobile primary factors
	rnum(r)		Numeraire region;

ne(i)$(not e(i)) = yes;
display ne;

parameters

*	Economic flows

	vfm(f,g,r)	Endowments - Firms' purchases at market prices,
	vdfm(i,g,r)	Intermediates - firms' domestic purchases at market prices,
	vifm(i,g,r)	Intermediates - firms' imports at market prices,
	vxmd(i,r,s)	Trade - bilateral exports at market prices,
	vst(i,r)	Trade - exports for international transportation
	vtwr(i,j,r,s)	Trade - Margins for international transportation at world prices

*	Tax and subsidy

	rto(g,r)	Output (or income) subsidy rates
	rtf(f,g,r)	Primary factor and commodity rates taxes 
	rtfd(i,g,r)	Firms domestic tax rates
	rtfi(i,g,r)	Firms' import tax rates
	rtxs(i,r,s)	Export subsidy rates
	rtms(i,r,s)	Import taxes rates

*	Energy flows

	evt(i,r,r)	Volume of energy trade (mtoe),
	evd(i,g,r)	Domestic energy use (mtoe),
	evi(i,g,r)	Imported energy use (mtoe),
	eco2d(i,g,r)	CO2 emissions in domestic fuels - Mt CO2",
	eco2i(i,g,r)	CO2 emissions in foreign fuels - Mt CO2"

*	GTAP elasticities

	esubd(i)	Elasticity of substitution (M versus D),
	esubva(g)	Elasticity of substitution between factors
	esubm(i)	Intra-import elasticity of substitution,
	etrae(f)	Elasticity of transformation,
*	eta(i,r)	Income elasticity of demand,
	epsilon(i,r)	Own-price elasticity of demand
;

$load vfm vdfm vifm vxmd vst vtwr 
$load rto rtf rtfd rtfi rtxs rtms 
$loaddc evd evi evt eco2d eco2i
$load esubd esubva esubm etrae epsilon


*-------------------------------------------------------------------------------
*	Consolidate Electricity Technologies on the Input Side
*-------------------------------------------------------------------------------

parameter	
	vdm(g,r)	Aggregate demand for domestic output,
	vom(g,r)	Total supply at market prices;

vdm(i,r) = sum(g, vdfm(i,g,r));
vom(i,r) = vdm(i,r) + sum(s, vxmd(i,r,s)) + vst(i,r);

parameter
	econ(i,g,r)	Aggregated energy consumption (quad)
	etrd(i,r,r)	Energy trade (quad)
	eprd(i,r)	Implied energy production (quad)
	eimp(i,r)	Energy import (quad)
	eexp(i,r)	Energy export (quad)
	etshr(*,r)	Share of electricity by technology in production
	vim(i,r)	Aggregate imports
	vafm(i,g,r)	Intermediate demand
	rtfa(i,g,r)	Tax on intermediate demand;

econ(i,g,r) = (evd(i,g,r) + evi(i,g,r))/quad2mtoe;
etrd(i,r,r) = evt(i,r,r)/quad2mtoe;
eprd(i,r) = sum(g,econ(i,g,r)) - sum(s,etrd(i,s,r)) + sum(s,etrd(i,r,s));
eimp(i,r) = sum(s,etrd(i,s,r));
eexp(i,r) = sum(s,etrd(i,r,s));

$onuni

*	Energy consumption and energy trade in Quad

econ("ele",g,r) = sum(et,econ(et,g,r));
etrd("ele",r,r) = sum(et,etrd(et,r,r));
eprd("ele",r) = sum(et,eprd(et,r));
eimp("ele",r) = sum(et,eimp(et,r));
eexp("ele",r) = sum(et,eexp(et,r));

etshr(et,r)$eprd("ele",r) = eprd(et,r)/eprd("ele",r);
etshr("ttl",r) = sum(et,etshr(et,r));
display etshr;

*	For reporting purpose, do not zero out eprd(et,r)
*eprd(et,r) = 0;
econ(et,g,r) = 0;
etrd(et,r,r) = 0;
eimp(et,r) = 0;
eexp(et,r) = 0;

*	Benchmark energy prices

parameter
	pe0(e,g,r)	Delivered energy prices ($ per MMBtu)
	ps0(e,r)	Wholesale energy prices ($ per MMBtu);

pe0(e,g,r)$econ(e,g,r) = (vdfm(e,g,r)+vifm(e,g,r))/econ(e,g,r);
ps0(e,r)$eprd(e,r)  = vom(e,r)/eprd(e,r);
ps0("ele",r)$eprd("ele",r) = sum(et,vom(et,r))/eprd("ele",r);
display pe0, ps0, vom, eprd;

*!!	There are eprd for CHN.GAS, JPN.COL, JPN.CRU, KOR.CRU but zero vom

parameter chkvome;
chkvome("noeprd",e,r)$(vom(e,r) and not eprd(e,r)) = yes;
chkvome("novom",e,r)$(not vom(e,r) and eprd(e,r)) = yes;
display chkvome;

*	Benchmark emissions

parameter	
	eco2		Carbon emissions
	cecphys		Carbon emission on physical value (MMTCO2 per Quad);

eco2(e,g,r) = eco2d(e,g,r) + eco2i(e,g,r);
eco2(e,"ttl",r) = sum(g,eco2d(e,g,r) + eco2i(e,g,r));
eco2("ttl","ttl",r) = sum((e,g),eco2d(e,g,r) + eco2i(e,g,r));
eco2("ttl","ttl","ttl") = sum((e,g,r),eco2d(e,g,r) + eco2i(e,g,r));
display eco2;

*!!	Some cases with eco2 but no econ

parameter chkco2e;
loop(e$(not ele(e)),
chkco2e("noeco2",e,g,r)$(econ(e,g,r) and not eco2(e,g,r)) = yes;
chkco2e("noecon",e,g,r)$(not econ(e,g,r) and eco2(e,g,r)) = yes;
);
display chkco2e;

loop(e$(not et(e)),
cecphys(e,g,r)$econ(e,g,r) = eco2(e,g,r)/econ(e,g,r);
cecphys(e,"avg",r)$(sum(g,econ(e,g,r)) and sum(g,eco2(e,g,r)))
	= sum(g$econ(e,g,r),eco2(e,g,r))/sum(g$eco2(e,g,r),econ(e,g,r));
);
display cecphys;


*	Aggregate domestic and imported electricity demand 
*	Derive weighted average tax

vdm("ele",r) = sum(et,vdm(et,r));
vdm(et,r) = 0;

vdfm("ele",g,r) = sum(et,vdfm(et,g,r));
rtfd("ele",g,r)$sum(et,vdfm(et,g,r)) 
	= sum(et,vdfm(et,g,r)*(1+rtfd(et,g,r)))
	/ sum(et,vdfm(et,g,r)) - 1;
vdfm(et,g,r) = 0;
rtfd(et,g,r) = 0;

vifm("ele",g,r) = sum(et,vifm(et,g,r));
rtfi("ele",g,r)$sum(et,vifm(et,g,r)) 
	= sum(et,vifm(et,g,r)*(1+rtfi(et,g,r)))
	/ sum(et,vifm(et,g,r)) - 1;
vifm(et,g,r) = 0;
rtfi(et,g,r) = 0;

*	Intermediate demand and corresponding tax

vafm(i,g,r) = vdfm(i,g,r) + vifm(i,g,r);
rtfa(i,g,r)$vafm(i,g,r) 
	= (vdfm(i,g,r)*(1+rtfd(i,g,r)) + vifm(i,g,r)*(1+rtfi(i,g,r)))
	/ vafm(i,g,r) - 1;	

*	Carbon coefficient 

*!!	Some cases with vafm but no eco2

parameter chkco2v;
loop(e$(not ele(e)),
chkco2v("noeco2",e,g,r)$(vafm(e,g,r) and not eco2(e,g,r)) = yes;
chkco2v("novafm",e,g,r)$(not vafm(e,g,r) and eco2(e,g,r)) = yes;
);
display chkco2v;

parameter	carbcoef	carbon coefficient on MMTCO2 per bn$;
carbcoef(e,g,r)$vafm(e,g,r) = eco2(e,g,r)/vafm(e,g,r); 
display carbcoef;


*	Trade flows and corresponding tariff rates

vim(i,r) = sum(g,vifm(i,g,r));
vxmd("ele",s,r) = sum(et,vxmd(et,s,r));
vtwr(j,"ele",s,r) = sum(et,vtwr(j,et,s,r));

rtxs("ele",s,r)$sum(et,vxmd(et,s,r))
	= 1 - sum(et,vxmd(et,s,r)*(1-rtxs(et,s,r)))
	/ sum(et,vxmd(et,s,r));

rtms("ele",s,r)$sum(et,vxmd(et,s,r)*(1-rtxs(et,s,r)))
	= (sum(et,vxmd(et,s,r)*(1-rtxs(et,s,r))*(1+rtms(et,s,r)))
	  + sum((et,j),vtwr(j,et,s,r)*(1+rtms(et,s,r)))
	)/ (sum(et,vxmd(et,s,r)*(1-rtxs(et,s,r)))
	    + sum((et,j),vtwr(j,et,s,r))) - 1;

vim(et,r) = 0;
vxmd(et,s,r) = 0;
vtwr(j,et,s,r) = 0;
rtxs(et,r,s) = 0;
rtms(et,r,s) = 0;

display vim;

$offuni


*	At this point, electrcity inputs are homogenous rather by technology
*	Electricity production is differentiated by technology


*-------------------------------------------------------------------------------
*	Reference tax and subsidies
*-------------------------------------------------------------------------------

parameter
	rtf0(f,g,r)	Primary factor and commodity rates taxes 
	rtfa0(i,g,r)	Reference tax on intermediate demand
	rtfd0(i,g,r)	Firms domestic tax rates
	rtfi0(i,g,r)	Firms' import tax rates
	rtxs0(i,r,s)	Export subsidy rates
	rtms0(i,r,s)	Import taxes rates;

rtf0(f,g,r) = rtf(f,g,r);
rtfa0(i,g,r) = rtfa(i,g,r);
rtfd0(i,g,r) = rtfd(i,g,r);
rtfi0(i,g,r) = rtfi(i,g,r);
rtxs0(i,r,s) = rtxs(i,r,s);
rtms0(i,r,s) = rtms(i,r,s);

parameter	
	pvxmd(i,s,r)	Import price (power of benchmark tariff)
	pvtwr(i,s,r)	Import price for transport services;

pvxmd(i,s,r) = (1+rtms0(i,s,r)) * (1-rtxs0(i,s,r));
pvtwr(i,s,r) = 1+rtms0(i,s,r);

parameter	
	vtw(j)		Aggregate international transportation services,
	vpm(r)		Aggregate private demand,
	vgm(r)		Aggregate public demand,
	evom(f,r)	Aggregate factor endowment at market prices,
	vb(*)		Current account balance;

vtw(j) = sum(r, vst(j,r));
vom("c",r) = sum(i, vdfm(i,"c",r)*(1+rtfd0(i,"c",r)) + vifm(i,"c",r)*(1+rtfi0(i,"c",r)))/(1-rto("c",r));
vom("g",r) = sum(i, vdfm(i,"g",r)*(1+rtfd0(i,"g",r)) + vifm(i,"g",r)*(1+rtfi0(i,"g",r)))/(1-rto("g",r));
vom("i",r) = sum(i, vdfm(i,"i",r)*(1+rtfd0(i,"i",r)) + vifm(i,"i",r)*(1+rtfi0(i,"i",r)))/(1-rto("i",r));

vdm("c",r) = vom("c",r);
vdm("g",r) = vom("g",r);
evom(f,r) = sum(g, vfm(f,g,r));
vb(r) = vom("c",r) + vom("g",r) + vom("i",r) 
	- sum(f, evom(f,r))
	- sum(g,  vom(g,r)*rto(g,r))
	- sum(g,  sum(i, vdfm(i,g,r)*rtfd(i,g,r) + vifm(i,g,r)*rtfi(i,g,r)))
	- sum(g,  sum(f, vfm(f,g,r)*rtf(f,g,r)))
	- sum((i,s), rtms(i,s,r) *  (vxmd(i,s,r) * (1-rtxs(i,s,r)) + sum(j,vtwr(j,i,s,r))))
	+ sum((i,s), rtxs(i,r,s) * vxmd(i,r,s));

vb("chksum") = sum(r, vb(r));
display vb;

*	Determine which factors are sector-specific 

mf(f) = yes$(1/etrae(f)=0);
sf(f) = yes$(1/etrae(f)>0);
display mf,sf;

parameter       
	mprofit		Zero profit for m,
        yprofit		Zero profit for y;

mprofit(i,r) = vim(i,r) - sum(s, pvxmd(i,s,r)*vxmd(i,s,r)+sum(j, vtwr(j,i,s,r))*pvtwr(i,s,r));
mprofit(i,r) = round(mprofit(i,r),5);
display mprofit;

yprofit(g,r) = vom(g,r)*(1-rto(g,r))
*	- sum(i, vafm(i,g,r)*(1+rtfa0(i,g,r)))
	- sum(i, vdfm(i,g,r)*(1+rtfd0(i,g,r))
               + vifm(i,g,r)*(1+rtfi0(i,g,r))) 
        - sum(f, vfm(f,g,r)*(1+rtf0(f,g,r)));

yprofit(g,r) = round(yprofit(g,r),6)
display yprofit;

*	Define a numeraire region for denominating international
*	transfers:

rnum(r) = yes$(vom("c",r)=smax(s,vom("c",s)));
display rnum;


*-------------------------------------------------------------------------------
*	Define model parameters
*-------------------------------------------------------------------------------

parameter
	tax0(r)		Reference tax revenue
	trnsf(r)	Reference transfer between household and government;

tax0(r) = sum(g,vom(g,r)*rto(g,r))
	+ sum((i,g),vafm(i,g,r)*rtfa(i,g,r))
	+ sum((f,g),vfm(f,g,r)*rtf(f,g,r))
	+ sum((i,s),vxmd(i,r,s)*(-rtxs(i,r,s)))
	+ sum((i,s),vxmd(i,s,r)*(1-rtxs(i,s,r))*rtms(i,s,r))
	+ sum((j,i,s),vtwr(j,i,s,r)*rtms(i,s,r));

trnsf(r) = vom("g",r) - tax0(r);

display tax0, trnsf;


parameter 
	a0(i,r)		Reference Armington supply
	d0(i,r)		Reference domestic aggregate supply
	m0(i,r)		Reference imported aggregate supply
	ele0(r)		Reference electricity production
	y0(g,r)		Reference output (electricity in total and by et)
	id0(i,g,r)	Reference intermediate demand
	kd0(g,r)	Reference capital demand
	ld0(g,r)	Reference labor demand
	rd0(g,r)	Reference resource demand
	lnd0(g,r)	Reference land demand

	pa0(i,g,r)	Reference aggregated intermediate input price
	pk0(g,r)	Reference kapital price
	pl0(g,r)	Reference labor price
	pr0(g,r)	Reference resource price
	pln0(g,r)	Reference land price

	ty(g,r)		Tax rate on output
	ta(i,g,r)	Tax rate on intermediate demand
	tk(g,r)		Tax rate on capital
	tl(g,r)		Tax rate on label
	tr(g,r)		Tax rate on resource
	tln(g,r)	Tax rate on land
	tx(i,r,r)	Export subsidy
	tm(i,r,r)	Import tax

	ke0(r)		Capital endowment
	le0(r)		Labor endowment
	re0(g,r)	Resource endowment
	lne0(g,r)	Land endowment
;

a0(i,r) = sum(g,vafm(i,g,r));
d0(i,r) = sum(g,vdfm(i,g,r));
m0(i,r) = sum(g,vifm(i,g,r));

ele0(r) = sum(g$et(g),vom(g,r));

*	y0 include electricity production by technology and in total
*	to simplify the model setup. The trade-off is on reporting.

y0(g,r) = vom(g,r);
y0("ele",r) = ele0(r);

id0(i,g,r) = vafm(i,g,r);
kd0(g,r) = vfm("cap",g,r);
ld0(g,r) = vfm("lab",g,r);
rd0(g,r) = vfm("fix",g,r);
lnd0(g,r) = vfm("lnd",g,r);

pa0(i,g,r) = 1 + rtfa0(i,g,r);
pk0(g,r) = 1 + rtf0("cap",g,r);
pl0(g,r) = 1 + rtf0("lab",g,r);
pr0(g,r) = 1 + rtf0("fix",g,r);
pln0(g,r) = 1 + rtf0("lnd",g,r);

ty(g,r) = rto(g,r);
ta(i,g,r) = rtfa(i,g,r);
tk(g,r) = rtf("cap",g,r);
tl(g,r) = rtf("lab",g,r);
tr(g,r) = rtf("fix",g,r);
tln(g,r) = rtf("lnd",g,r);
tx(i,r,s) = rtxs(i,r,s);
tm(i,r,s) = rtms(i,r,s);

ke0(r) = evom("cap",r);
le0(r) = evom("lab",r);
re0(g,r) = rd0(g,r);
lne0(g,r) = lnd0(g,r);


*-------------------------------------------------------------------------------
*	ELASTICITY ASSUMPTIONS
*-------------------------------------------------------------------------------

parameter	esub(g)         Top-level elasticity indemand / C 1 /;


*-------------------------------------------------------------------------------
*	EMF 34 Global Model - Recursive Dynamic
*-------------------------------------------------------------------------------

$ontext
$model:core

$peps:

$sectors:
        y(g,r)$(y0(g,r)$(not sameas(g,"ele")))		! Supply
	elec(r)$ele0(r)					! Electricity production
	a(i,r)$a0(i,r)					! Armington supply
        m(i,r)$m0(i,r)					! Imports
        yt(j)$vtw(j)					! Transportation services
        ksup(r)$ke0(r)					! Capital supply
        lsup(g,r)$ld0(g,r)				! Labor supply

$commodities:
        p(g,r)$y0(g,r)					! Domestic output price
	pa(i,r)$a0(i,r)					! Armington price
        pm(j,r)$m0(j,r)					! Import price
        pt(j)$vtw(j)					! Transportation services
        pk(r)$ke0(r)					! Capital rent
	pl(g,r)$ld0(g,r)				! Labor wage rate
	pr(g,r)$rd0(g,r)				! Resource rent
        pln(g,r)$lnd0(g,r)				! Land price
        pks(r)$ke0(r)					! Price for capital
	pls(r)$le0(r)					! Price for labor
	ptax(r)						! Index of tax revenue

$consumers:
        ra(r)						! Representative agent
	gov(r)						! Representative government
	irs(r)						! Tax collection

$prod:y(g,r)$(y0(g,r)$(not sameas(g,"ele")))   s:esub(g)    i.tl:esubd(i)  va:esubva(g)
        o:p(g,r)        q:y0(g,r)				a:irs(r) t:ty(g,r)
        i:pa(i,r)	q:id0(i,g,r)	p:pa0(i,g,r)	i.tl:	a:irs(r) t:ta(i,g,r)
        i:pk(r)		q:kd0(g,r)	p:pk0(g,r)	va:	a:irs(r) t:tk(g,r)
        i:pl(g,r)	q:ld0(g,r)	p:pl0(g,r)	va:	a:irs(r) t:tl(g,r)
        i:pr(g,r)	q:rd0(g,r)	p:pr0(g,r)	va:	a:irs(r) t:tr(g,r)
        i:pln(g,r)	q:lnd0(g,r)	p:pln0(g,r)	va:	a:irs(r) t:tln(g,r)

$prod:elec(r)$ele0(r)
	o:p("ele",r)	q:ele0(r)
	i:p(et,r)	q:y0(et,r)

$prod:a(i,r)$a0(i,r)	s:1
	o:pa(i,r)	q:a0(i,r)	
        i:p(i,r)	q:d0(i,r)	
        i:pm(i,r)	q:m0(i,r)	

$prod:yt(j)$vtw(j)	s:1
        o:pt(j)         q:vtw(j)
        i:p(j,r)        q:vst(j,r)

$prod:m(i,r)$m0(i,r)   s:esubm(i)  s.tl:0
        o:pm(i,r)	q:m0(i,r)
        i:p(i,s)	q:vxmd(i,s,r)   p:pvxmd(i,s,r) s.tl: 
+			a:irs(s) t:(-tx(i,s,r)) a:irs(r) t:(tm(i,s,r)*(1-tx(i,s,r)))
        i:pt(j)#(s)	q:vtwr(j,i,s,r) p:pvtwr(i,s,r) s.tl: a:irs(r) t:tm(i,s,r)

$prod:ksup(r)$ke0(r)  t:etrae("cap")
        o:pk(r)		q:ke0(r)
        i:pks(r)	q:ke0(r)

$prod:lsup(g,r)$ld0(g,r)  t:etrae("lab")
        o:pl(g,r)	q:ld0(g,r)
        i:pls(r)	q:ld0(g,r)

$demand:ra(r)
        d:p("c",r)      q:vom("c",r)
        e:p("i",r)      q:(-vom("i",r))
	e:pks(r)	q:ke0(r)
	e:pls(r)	q:le0(r)
	e:pr(g,r)	q:rd0(g,r)
	e:pln(g,r)	q:lnd0(g,r)
        e:p("c",rnum)   q:vb(r)
	e:p("c",rnum)	q:(-trnsf(r))

$demand:irs(r)
	d:ptax(r)		

$demand:gov(r)
	d:p("g",r)      q:y0("g",r)
	e:ptax(r)	q:tax0(r)
	e:p("c",rnum)	q:trnsf(r)

$report:

	v:id(i,g,r)		i:pa(i,r)	prod:y(g,r)
	v:kd(g,r)		i:pk(r)		prod:y(g,r)
	v:ld(g,r)		i:pl(g,r)	prod:y(g,r)
	v:rd(g,r)		i:pr(g,r)	prod:y(g,r)
	v:lnd(g,r)		i:pln(g,r)	prod:y(g,r)

	v:ad(i,r)		i:p(i,r)	prod:a(i,r)
	v:am(i,r)		i:pm(i,r)	prod:a(i,r)

	v:trade(i,s,r)		i:p(i,s)	prod:m(i,r)
	v:vstd(j,r)		i:p(j,r)	prod:yt(j)

$offtext
$sysinclude mpsgeset core


*	Benchmark replication

core.workspace = 256;
core.iterlim = 0;
core.optfile = 1;
$include core.gen
solve core using mcp;
