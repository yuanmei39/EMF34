$title   Recursive-Dynamic Model for EMF 34 Trilateral Study

*	Mei Yuan
*	yuanmei@mit.edu

*	10/09/2017	Add sets definition, model parameters
*	10/12/2017	Add nesting structure

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
	ele(g)		Electricity and by type		/ set.et_, ele /
	elec(g)		Electricity			/ ele /
	et(g)		Generation technologies		/ set.et_ /
	nuc(g)		Nuclear generation		/ enuc /
	hyd(g)		Hydro generation		/ ehyd /
	ws(et)		Wind and solar			/ ewnd, eslr /
	
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
	ag		All sectors for reporting	/ set.g, set.hhtrn /
	aeeis(g)	Agriculture and energy and energy intensive goods
	net(g)		Non-energy and non-tnd goods
;

aeeis(g)$(e(g) or agr(g) or eis(g)) = yes;
net(g)$(not e(g) and not tnd(g)) = yes;

alias (r,s), (i,j), (g,gg), (f,ff);

display ag, net;

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
*	esubva(g)	Elasticity of substitution between factors
	esubm(i)	Intra-import elasticity of substitution,
	etrae(f)	Elasticity of transformation,
*	eta(i,r)	Income elasticity of demand,
	epsilon(i,r)	Own-price elasticity of demand
;

$load vfm vdfm vifm vxmd vst vtwr 
$load rto rtf rtfd rtfi rtxs rtms 
$loaddc evd evi evt eco2d eco2i
$load esubd esubm etrae epsilon


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

*-------------------------------------------------------------------------------
*	Carbon coefficient 
*-------------------------------------------------------------------------------

*	Benchmark emissions

parameter	eco2		Carbon emissions (MMTCO2);
eco2(e,g,r) = eco2d(e,g,r) + eco2i(e,g,r);
eco2(e,"ttl",r) = sum(g,eco2d(e,g,r) + eco2i(e,g,r));
eco2("ttl","ttl",r) = sum((e,g),eco2d(e,g,r) + eco2i(e,g,r));
eco2("ttl","ttl","ttl") = sum((e,g,r),eco2d(e,g,r) + eco2i(e,g,r));
display eco2;

parameter	cecphys		Carbon emission coefficient (MMTCO2 per Quad);
loop(e$(not et(e)),
cecphys(e,g,r)$econ(e,g,r) = eco2(e,g,r)/econ(e,g,r);
cecphys(e,"avg",r)$(sum(g,econ(e,g,r)) and sum(g,eco2(e,g,r)))
	= sum(g$econ(e,g,r),eco2(e,g,r))/sum(g$eco2(e,g,r),econ(e,g,r));
);
display cecphys;

*	Some cases with eco2 but no econ or economic values
*	These cases contains negligible values except for CRU in OIL.
*	Assume no emissions from CRU use in OIL.

parameter	chkco2e;
loop(e$(not ele(e)),
chkco2e("noeco2",e,g,r)$(econ(e,g,r) and not eco2(e,g,r)) = econ(e,g,r);
chkco2e("noecon",e,g,r)$(not econ(e,g,r) and eco2(e,g,r)) = eco2(e,g,r);
);
display chkco2e;

*	Some cases with vafm and econ but no eco2
*	These cases contains negligible values except for CRU in OIL.
*	Assume no emissions from CRU use in OIL.

parameter	chkco2v;
loop(e$(not ele(e)),
chkco2v("noeco2",e,g,r)$(vafm(e,g,r) and not eco2(e,g,r)) = vafm(e,g,r);
chkco2v("novafm",e,g,r)$(not vafm(e,g,r) and eco2(e,g,r)) = eco2(e,g,r);
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
	id0(*,g,r)	Reference intermediate demand
	kd0(g,r)	Reference capital demand
	ld0(g,r)	Reference labor demand
	rd0(g,r)	Reference resource demand
	lnd0(g,r)	Reference land demand
	c0(r)		Reference private consumption
	gov0(r)		Reference government consumption
	inv0(r)		Reference investment goods
	w0		Reference welfare

	pa0(*,g,r)	Reference aggregated intermediate input price
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

c0(r) = vom("c",r);
gov0(r) = vom("g",r);
inv0(r) = vom("i",r);
w0(r) = c0(r) + inv0(r);
y0(fd,r) = 0;

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

parameter tndshr	electricitye T&D cost share;
tndshr("prd",et,r)$y0(et,r) = id0("tnd",et,r)/y0(et,r);
tndshr("prd","avg",r) = sum(et,id0("tnd",et,r))/sum(et,y0(et,r));
tndshr("con",g,r)$id0("ele",g,r) = id0("tnd",g,r)/id0("ele",g,r);
tndshr("con","avg",r)$sum(g,id0("ele",g,r)) = sum(g,id0("tnd",g,r))/sum(g,id0("ele",g,r));
display tndshr;

*	Take TnD as a cost markup accounting for electricity delivery margin

id0("etd",g,r) = id0("ele",g,r)+id0("tnd",g,r);

pa0("etd",g,r)$(id0("ele",g,r)+id0("tnd",g,r)) 
	= (id0("ele",g,r)*(1+ta("ele",g,r))+id0("tnd",g,r)*(1+ta("tnd",g,r)))
	/ (id0("ele",g,r)+id0("tnd",g,r));


*-------------------------------------------------------------------------------
*	Other Set Definitions for Policy Implementation
*-------------------------------------------------------------------------------

*	Regions:

set	oecd(r)		/usa, can, jpn, eur, anz, kor/
	ldc(r)		/mex, roe, rus, asi, chn, ind, bra, afr, mes, lam, rea, kor, idz/
	ldc_rich(ldc)	/mex, bra, asi, lam, kor, idz/
	ldc_poor(ldc)	/afr, ind, rea/
	ldc_mes(ldc)	/mes/
	eu(r)		/eur, roe/
	us(r)		/usa/
	usa(r)		/usa/
	jpn(r)		/jpn/
	rus(r)		/rus/
	cane(r)		/afr, ind, bra, lam, mex, anz, rea/
	beet(r)		/eur, roe, usa, rus, jpn, chn, mes/
	rape(r)		/can, eur, roe, chn,  anz, ind, usa, afr, lam/
	soy(r)		/usa, can, mex,jpn, anz, lam, bra, chn, asi, ind, afr, rea, mes, kor, idz/
	palm(r)		/afr, asi, kor, idz/
	cornre(r)	/afr, can, chn, roe, eur, rea, lam, mex, asi, usa, mes, ind, anz, rus, bra, kor, idz/
	wheatre(r)	/mex, mes, lam, usa, afr, anz, can, chn, roe, ind, jpn, rus, eur, bra/

	devlp(r)	Developed regions /usa, can, eur, anz, jpn/
	otg20(r)	Other G20 regions /mex, rus, bra, chn, ind, asi, kor, idz/
	restw(r)	Rest of the world /roe, afr, mes, lam, rea/;

set	mkt		Natural gas markets /NAM, URB, AUS/;

set     gmap(r,mkt)	Maping gas markets /
			USA.NAM	United States
			CAN.NAM	Canada
			MEX.NAM	Mexico
			JPN.AUS	Japan
			ANZ.AUS	Australia - New Zealand
			EUR.URB	Europe
			ROE.URB	Eurasia
			RUS.AUS	Russia
			ASI.AUS	East Asia
			CHN.AUS	China	
			IND.AUS	India
			BRA.NAM	Brazil
			AFR.URB	Africa
			MES.URB	Middle East
			LAM.NAM	Latin America
			REA.AUS	Rest of the World
			kor.aus S. Korea
			idz.aus Indonesia /;

set	grt		Gas resource types / 
			cnv conventional, 
			shl shale, 
			tgh tight sands, 
			cbm coal bed methane/;

alias (grt,gtr);


*-------------------------------------------------------------------------------
*	Greenhouse Gases:
*-------------------------------------------------------------------------------

set	ghg		Green house gas pollutants / CH4, N2O, PFC, SF6, HFC /
	lgh(ghg)	Long lived gases	   / PFC, SF6, HFC /
	hfc(ghg)	/ HFC /
	ch4(ghg)	/ CH4 /
	n2o(ghg)	/ N2O /
	urb		Non_ghg pollutants / CO, VOC, NOX, SO2, BC, OC, amo /
	boc(urb)	/ BC, OC, amo /
	bc_(urb)	/ BC, OC /;

set	ghgk(r)		National non-CO2 constrained ghg gases
	ghgkw(r)	International non-CO2 constrained ghg gases
	ghgks(r)	Sectoral non-CO2 constrained ghg gases -- sectorial policy
        urbn(urb,r)	National non-GHG constraints flag
        co2c(r)		CO2-constrained regions (non-trading)
	sco2c(r)	CO2-constrained regions -- sectorial policy (non-trading)
	tco2c(r)	Tradable CO2-contrained regions
	ss		Sector specific ghg policy;

ghgk(r)		= no;
ghgkw(r)	= no;
ghgks(r)	= no;
urbn(urb,r)	= no;
co2c(r)		= no;
sco2c(r)	= no; 
tco2c(r)	= no;
ss(r,s)		= no;

parameter	
	wghgk		Flag to activate cross-country trading of ghg gases;

wghgk = no;

set	co2cf(r,t)	Control for co2c - regional CO2 constraint
	tco2cf(r,t)	Control for tco2c - regions with international CO2 cap-and-trade
	sco2cf(r,t)	Control for sco2c - regions with sectoral CO2 constraints  
	ghgkf(r,t)	Control for ghgk - regional ghg constraint
	ghgksf(r,t)	Control for ghgks - regions with sectoral ghg constraints
	ghgkwf(r,t)	Control for ghgkw - regions with international GHG cap-and-trade
	urbnf(urb,r,t)	Control for urbn - regions with non-GHG constraint on urb
	cflag(r)	Flag for deforestation and eint (cement) emissions
	cflagf(r,t)	Control for cflag - deforestation and eint (cement) emissions;

co2cf(r,t)     = no;
tco2cf(r,t)    = no;
sco2cf(r,t)    = no;
ghgkf(r,t)     = no;
ghgksf(r,t)    = no;
ghgkwf(r,t)    = no;
urbnf(urb,r,t) = no;
cflag(r)       = no;
cflagf(r,t)    = no;

parameter	
	carblim(r)		Non-tradable CO2 emisssion permits
	ghglim(ghg,r)		Policy limit on ghg gas emissions
	ghglimg(ghg,*,r)	Policy limit on ghg gas emissions by sector;

carblim(r) = 0;
ghglim(ghg,r) = 0;
ghglimg(ghg,g,r) = 0;   

parameter
	ghginv			ghg inventory from data files ghg_inv and ghg_trend
	curb0(urb,*,*,r)	Non-greenhouse gases (associated with consumption)
	ourb0(urb,*,r)		Non-greenhouse gases (associated with production)
	cghg(*,*,*,r)		Greenhouse gases (associated with consumption)
	oghg(*,*,r)		Greenhouse gases (associated with production);


ghginv(g,ghg,r) = 0;
cghg(ghg,e,g,r) = 0;
oghg(ghg,g,r) = 0;


*-------------------------------------------------------------------------------
*	ELASTICITY ASSUMPTIONS
*-------------------------------------------------------------------------------

*	Production sector elasticities:

parameter	
	esubr(g,r)	Elasticity of substitution between resource and other inputs
	esuba(*,r)	Elasticity of substitution (nest a)
	esubeva(*,r)	Elasticity of substitution between energy and value-added
	esube(*,r)	Elasticity of substitution between electricity and fossil fuels
	esubf(*,r)	Elasticity of substitution between fossil fuels
	esubva(*,r)	Elasticity of substitution between capital and labor
	esubews(r)	Elasticity of substitution between different types of generation
;

esubr(xe,r) = 0.5;
esubr(agr,r) = 0.3;
esubr(eis,r) = 0.3;
esubr(oth,r) = 0.3;
esubr(trn,r) = 0.3;

esubr(et,r) = 0.6;

esuba(g,r)   = 0;
esuba(agr,r) = 0.7;

esubeva(e,r) = 0.8;
esubeva(ne,r) = 1.0;
esubeva(agr,r) = 0.6;

esube(g,r) = 1.5;
esube("c",r) = 1.5;

esubf(g,r) = 1;
esubva(g,r) = 1;

esubews(r) = 0.5;

*	Armington trade elasticities:

parameter 
	esubdm(g,r)	Elasticity of substitution between domestic and imported goods
	esubmm(g,r)	Elasticity of substitution among imports;

esubdm(e,r) = 1.5;
esubdm(ne,r) = 3.0;

esubmm(e,r) = 3.0;
esubmm(ne,r) = 5.0;

*	Final demand elasticities:

parameter	
	eta(r,g)	Stone-Geary income elasticity of g
	delas(fd,r)	Final demand elasticity between energy and non-energy composites
	sigtrn		Elasticity between transport consumption and other consumption in final demand;

eta(r,g) = 1;
delas("c",r) = 0.25;
delas("g",r) = 0.25;
sigtrn = 0.5;

*	Incorporate Supria estimates of demand elasticity

parameter d_elas(r)	Top final demand substitution elasticity /
	USA	0.65
	CAN	0.65
	MEX	0.55
	JPN	0.69
	EUR	0.62
	ANZ	0.61
	ROE	0.31
	RUS	0.26
	ASI	0.38
	CHN	0.30
	IND	0.25
	BRA	0.35
	AFR	0.25
	MES	0.32
	LAM	0.41
	REA	0.25 /;

* Multiple gases:

parameter 
	sigu(g,r)	Top level transformation elasticity between production and urban gases
	sigg(ghg,g,r)	Elasticity of substitution for ghg 
	siggv(g,r)	Elasticity of substitution for ghg in vintaged sectors;

*	Define elasticity structure for urban gases:

sigu(g,r) = 0;
sigu(fd,r) = 0;

*	These elasticities are based on fitting MAC curves to EPA data

*	CH4:
sigg("ch4",gas,r) = 0.15;
sigg("ch4",cru,r) = 0.15;
sigg("ch4",col,r) = 0.30;
sigg("ch4",eis,r) = 0.11;
sigg("ch4",oth,r) = 0.11;
sigg("ch4",srv,r) = 0.11;
sigg("ch4",trn,r) = 0.11;
sigg("ch4",fd,r)  = 0.11;

sigg("ch4",agr,"usa") = 0.05;
sigg("ch4",agr,"jpn") = 0.07;
sigg("ch4",agr,"eur") = 0.07;
sigg("ch4",agr,"anz") = 0.04;
sigg("ch4",agr,"rus") = 0.05;
sigg("ch4",agr,"roe") = 0.08;
sigg("ch4",agr,"chn") = 0.05;
sigg("ch4",agr,"ind") = 0.04;
sigg("ch4",agr,"mex") = 0.04;
sigg("ch4",agr,"mes") = 0.02;
sigg("ch4",agr,"bra") = 0.02;
sigg("ch4",agr,"lam") = 0.02;
sigg("ch4",agr,"asi") = 0.05;
sigg("ch4",agr,"rea") = 0.03;

sigg("ch4",agr,r)$(not sigg("ch4",agr,r)) = 0.03;


*	The following elasticity values are based on Jochen work

*	N2O:
sigg("n2o",agr,r)=0.04;
sigg("n2o",agr,oecd)=0.04;
sigg("n2o",agr,ldc)=0.02;
sigg("n2o",agr,"rus")=0.04;
sigg("n2o",agr,"roe")=0.04;
sigg("n2o",col,r)=0;
sigg("n2o",oil,r)=0;
sigg("n2o",eis,r)=1;

*	PFC:
sigg("pfc",eis,r)=0.3;
sigg("pfc",oth,r)=0.3;

*	HFC:
sigg("hfc",oth,r)=0.15;

*	SF6:
sigg("sf6",eis,r)=0.3;
sigg("sf6",ele,r)=0.3;

*	For CCSP - double

sigg("sf6",g,r) = 2*sigg("sf6",g,r);
sigg("pfc",g,r) = 2*sigg("pfc",g,r);
sigg("hfc",g,r) = 4*sigg("hfc",g,r);

*	Vintaged sectors:
*siggv(agr,r) = 0.01;
siggv(eis,r) = 0.15;
siggv(oth,r) = 0.15;
siggv(trn,r) = 0.15;
siggv(ele,r) = 0.10;


*	Black carbon and organic carbon elasticities:

parameter	
	s_bc(g,r)	Sectoral elasticity of substitution for black and organic carbon
	bsigma(r)	Elasticity of substitution for biomass generation
	boilsig(r)	Fixed factor elasticity for 2nd gen bio-oil
	boilffg(r)	Fixed factor elasticity for 1st gen bio-oil;

s_bc(g,r)=0;
s_bc(eis,r)=0.5;

bsigma(r) = 0.3;
boilsig(r) = 0.1;
boilffg(r) = 0.1;


*-------------------------------------------------------------------------------
*	EMF 34 Global Model - Recursive Dynamic
*-------------------------------------------------------------------------------

$ontext
$model:core

$peps:

$sectors:
	w(r)							! Welfare index
	c(r)							! Private consumption
	gov(r)							! Government consumption
	inv(r)							! Investment goods production
        y(g,r)$(y0(g,r)$(not sameas(g,"ele")))			! Supply
	yele(r)$ele0(r)						! Electricity production
	a(i,r)$a0(i,r)						! Armington supply
        m(i,r)$m0(i,r)						! Imports
        yt(j)$vtw(j)						! Transportation services
        ksup(r)$ke0(r)						! Capital supply
        lsup(g,r)$ld0(g,r)					! Labor supply
	ed(i,g,r)$(id0(i,g,r)$enoe(i))				! Regional delivered energy by fuel and sector
	enws(et,r)$(y0(et,r)$(not ws(et)))			! Non-renewable electricity generation
	ews(et,r)$(y0(et,r)$ws(et))				! Renewable electricity generation
	eled(g,r)$id0("ele",g,r)				! Regional delivered electricity by sector

$commodities:
	pw(r)							! Welfare price index
	pc(r)							! Private consumption price index
	pgov(r)							! Government price index
	pinv(r)							! Investment price index
        p(g,r)$y0(g,r)						! Domestic output price
	pa(i,r)$a0(i,r)						! Armington price
	pe(i,g,r)$(id0(i,g,r)$e(i))				! Energy price
        pm(j,r)$m0(j,r)						! Import price
        pt(j)$vtw(j)						! Transportation services
        pk(r)$ke0(r)						! Capital rent
	pl(g,r)$ld0(g,r)					! Labor wage rate
	pr(g,r)$rd0(g,r)					! Resource rent
        pln(g,r)$lnd0(g,r)					! Land price
        pks(r)$ke0(r)						! Price for capital
	pls(r)$le0(r)						! Price for labor
	ptax(r)							! Index of tax revenue
	pnws(r)$sum(et$(not ws(et)),y0(et,r))			! Non-renewable electricity price
	pws(r)$sum(et$ws(et),y0(et,r))				! Renewable electricity price

	pcarbr(r)$carblim(r)					! CO2 permit price -- regional
	pghg(ghg,r)$(ghgk(r)$ghglim(ghg,r))			! GHG price -- regional 
	pghgw(ghg)$(wghgk$sum(r,ghglim(ghg,r)))			! GHG price -- national trading
	pghgs(ghg,g,r)$(ghgks(r)$(ss(g,r)$ghglimg(ghg,g,r)))	! GHG price -- sector 

$consumers:
        ra(r)							! Representative agent
	govt(r)							! Representative government
	irs(r)							! Tax collection

*	GHG INCLUSIVE DELIVERED ENERGY:
$prod:ed(i,g,r)$(id0(i,g,r)$enoe(i))
	o:pe(i,g,r)						q:id0(i,g,r)
	i:pa(i,r)						q:id0(i,g,r)
	i:pcarbr(r)$carblim(r)					q:(id0(i,g,r)*carbcoef(i,g,r))		
	i:pghg(ghg,r)$(ghglim(ghg,r)$(not wghgk))		q:cghg(ghg,i,g,r)
	i:pghgw(ghg)$(ghglim(ghg,r)$wghgk)			q:cghg(ghg,i,g,r)
	i:pghgs(ghg,g,r)$(ghgks(r)$(ss(g,r)$ghglimg(ghg,g,r)))	q:cghg(ghg,i,g,r)

*	ELECTRICITY INCLUSIVE OF TRANSMISSION AND DISTRIBUTION
$prod:eled(g,r)$id0("ele",g,r)   
	o:pe("ele",g,r)		q:id0("etd",g,r)	p:pa0("etd",g,r)
	i:pa("ele",r)		q:id0("ele",g,r)	p:pa0("ele",g,r)	a:irs(r) t:ta("ele",g,r)				
	i:pa("tnd",r)		q:id0("tnd",g,r)	p:pa0("tnd",g,r)	a:irs(r) t:ta("tnd",g,r)				

*	GENERAL DOMESTIC PRODUCTION:
$prod:y(g,r)$(y0(g,r)$(not sameas(g,"ele"))$(not aeeis(g)))   
+	s:esubr(g,r)  a:esuba(g,r)  eva(a):esubeva(g,r)  va(eva):esubva(g,r)
+	e(eva):esube(g,r)  ele(e):0  ef(e):esubf(g,r)
+	enoe.tl(ef):0

        o:p(g,r)		q:y0(g,r)					a:irs(r) t:ty(g,r)
        i:pr(g,r)		q:rd0(g,r)		p:pr0(g,r)		a:irs(r) t:tr(g,r)
        i:pa(i,r)$net(i)	q:id0(i,g,r)		p:pa0(i,g,r)	a:	a:irs(r) t:ta(i,g,r)
        i:pe(i,g,r)$elec(i)	q:id0("etd",g,r)	p:pa0("etd",g,r) ele:	
        i:pe(i,g,r)$enoe(i)	q:id0(i,g,r)		p:pa0(i,g,r)	ef:	a:irs(r) t:ta(i,g,r)
        i:pk(r)			q:kd0(g,r)		p:pk0(g,r)	va:	a:irs(r) t:tk(g,r)
        i:pl(g,r)		q:ld0(g,r)		p:pl0(g,r)	va:	a:irs(r) t:tl(g,r)

*	ENERGY INTENSIVE GOODS:
$prod:y(g,r)$(y0(g,r)$(not sameas(g,"ele"))$eis(g))
+	s:esubr(g,r)  lr:0  a:esuba(g,r)  eva(a):esubeva(g,r)  va(eva):esubva(g,r)
+	e(eva):esube(g,r)  ele(e):0  ef(e):esubf(g,r)

        o:p(g,r)		q:y0(g,r)					a:irs(r) t:ty(g,r)
        i:pln(g,r)		q:lnd0(g,r)		p:pln0(g,r)	lr:	a:irs(r) t:tln(g,r)
        i:pr(g,r)		q:rd0(g,r)		p:pr0(g,r)	lr:	a:irs(r) t:tr(g,r)
        i:pa(i,r)$net(i)	q:id0(i,g,r)		p:pa0(i,g,r)	a:	a:irs(r) t:ta(i,g,r)
        i:pe(i,g,r)$elec(i)	q:id0("etd",g,r)	p:pa0("etd",g,r) ele:	
        i:pe(i,g,r)$enoe(i)	q:id0(i,g,r)		p:pa0(i,g,r)	ef:	a:irs(r) t:ta(i,g,r)
        i:pk(r)			q:kd0(g,r)		p:pk0(g,r)	va:	a:irs(r) t:tk(g,r)
        i:pl(g,r)		q:ld0(g,r)		p:pl0(g,r)	va:	a:irs(r) t:tl(g,r)

*	AGRICULTURE:
$prod:y(g,r)$(y0(g,r)$(not sameas(g,"ele"))$agr(g))
+	a:esuba(g,r)  va(a):esubva(g,r)  emrl(a):esubr(g,r)  lr(emrl):0
+	e(emrl):esube(g,r)  m(e):0  ele(e):0  ef(e):esubf(g,r)

        o:p(g,r)		q:y0(g,r)					a:irs(r) t:ty(g,r)
        i:pln(g,r)		q:lnd0(g,r)		p:pln0(g,r)	lr:	a:irs(r) t:tln(g,r)
        i:pr(g,r)		q:rd0(g,r)		p:pr0(g,r)	lr:	a:irs(r) t:tr(g,r)
        i:pa(i,r)$net(i)	q:id0(i,g,r)		p:pa0(i,g,r)	m:	a:irs(r) t:ta(i,g,r)
        i:pe(i,g,r)$elec(i)	q:id0("etd",g,r)	p:pa0("etd",g,r) ele:	
        i:pe(i,g,r)$enoe(i)	q:id0(i,g,r)		p:pa0(i,g,r)	ef:	a:irs(r) t:ta(i,g,r)
        i:pk(r)			q:kd0(g,r)		p:pk0(g,r)	va:	a:irs(r) t:tk(g,r)
        i:pl(g,r)		q:ld0(g,r)		p:pl0(g,r)	va:	a:irs(r) t:tl(g,r)

*	ELECTRICITY GENERATION BY TECHNOLOGY TYPE 
*	TRANSMISSION AND DISTRIBUTION AS A FIXED COST MARKUP:
$prod:y(g,r)$(y0(g,r)$et(g))
+	s:esubr(g,r)  a:esuba(g,r)  m(a):0  eva(a):esubeva(g,r)  va(eva):esubva(g,r)
+	ef(eva):0

        o:p(g,r)		q:y0(g,r)					a:irs(r) t:ty(g,r)
        i:pr(g,r)		q:rd0(g,r)		p:pr0(g,r)	sr:	a:irs(r) t:tr(g,r)
        i:pa(i,r)$net(i)	q:id0(i,g,r)		p:pa0(i,g,r)	m:	a:irs(r) t:ta(i,g,r)
        i:pe(i,g,r)$elec(i)	q:id0("etd",g,r)	p:pa0("etd",g,r) ef:	
        i:pe(i,g,r)$enoe(i)	q:id0(i,g,r)		p:pa0(i,g,r)	ef:	a:irs(r) t:ta(i,g,r)
        i:pk(r)			q:kd0(g,r)		p:pk0(g,r)	va:	a:irs(r) t:tk(g,r)
        i:pl(g,r)		q:ld0(g,r)		p:pl0(g,r)	va:	a:irs(r) t:tl(g,r)

*	AGGREGATE DOMESTIC ELECTRICITY PRODUCTION:
$prod:yele(r)$ele0(r)	s:esubews(r)
	o:p("ele",r)	q:ele0(r)
	i:pnws(r)	q:(sum(et$(not ws(et)),y0(et,r)))
	i:pws(r)	q:(sum(et$ws(et),y0(et,r)))

*	NON-RENEWABLE ELECTRICITY GENERATION
$prod:enws(et,r)$(y0(et,r)$(not ws(et)))
	o:pnws(r)	q:y0(et,r)
	i:p(et,r)	q:y0(et,r)

*	RENEWABLE ELECTRICITY GENERATION
$prod:ews(et,r)$(y0(et,r)$ws(et))
	o:pws(r)	q:y0(et,r)
	i:p(et,r)	q:y0(et,r)

*	EXHAUSTIBLE FUEL PRODUCTION:
$prod:y(g,r)$(y0(g,r)$xe(g))
+	s:esubr(g,r)  a:esuba(g,r)  m(a):0  va(a):esubva(g,r)  
+	e(a):esube(g,r)  ele(e):0  ef(e):esubf(g,r)

        o:p(g,r)		q:y0(g,r)					a:irs(r) t:ty(g,r)
        i:pr(g,r)		q:rd0(g,r)		p:pr0(g,r)		a:irs(r) t:tr(g,r)
        i:pa(i,r)$net(i)	q:id0(i,g,r)		p:pa0(i,g,r)	m:	a:irs(r) t:ta(i,g,r)
        i:pe(i,g,r)$elec(i)	q:id0("etd",g,r)	p:pa0("etd",g,r) ele:	
        i:pe(i,g,r)$enoe(i)	q:id0(i,g,r)		p:pa0(i,g,r)	ef:	a:irs(r) t:ta(i,g,r)
        i:pk(r)			q:kd0(g,r)		p:pk0(g,r)	va:	a:irs(r) t:tk(g,r)
        i:pl(g,r)		q:ld0(g,r)		p:pl0(g,r)	va:	a:irs(r) t:tl(g,r)

*	REFINED OIL PRODUCTION:
$prod:y(g,r)$(y0(g,r)$oil(g))
+	s:0  sr:esubr(g,r)  a:esuba(g,r)  m(a):0  va(a):esubva(g,r)  
+	e(a):esube(g,r)  ele(e):0  ef(e):esubf(g,r)

        o:p(g,r)		q:y0(g,r)					a:irs(r) t:ty(g,r)
        i:pe(i,g,r)$cru(i)	q:id0(i,g,r)		p:pa0(i,g,r)		a:irs(r) t:ta(i,g,r)
        i:pr(g,r)		q:rd0(g,r)		p:pr0(g,r)	sr:	a:irs(r) t:tr(g,r)
        i:pa(i,r)$net(i)	q:id0(i,g,r)		p:pa0(i,g,r)	m:	a:irs(r) t:ta(i,g,r)
        i:pe(i,g,r)$elec(i)	q:id0("etd",g,r)	p:pa0("etd",g,r) ele:	
        i:pe(i,g,r)$fe(i)	q:id0(i,g,r)		p:pa0(i,g,r)	ef:	a:irs(r) t:ta(i,g,r)
        i:pk(r)			q:kd0(g,r)		p:pk0(g,r)	va:	a:irs(r) t:tk(g,r)
        i:pl(g,r)		q:ld0(g,r)		p:pl0(g,r)	va:	a:irs(r) t:tl(g,r)

*	ARMINGTON GOODS:
$prod:a(i,r)$a0(i,r)	s:esubdm(i,r)
	o:pa(i,r)	q:a0(i,r)	
        i:p(i,r)	q:d0(i,r)		
        i:pm(i,r)	q:m0(i,r)	

*	INTERNATIONAL TRANSPORTATION SERVICES:
$prod:yt(j)$vtw(j)	s:1
        o:pt(j)         q:vtw(j)
        i:p(j,r)        q:vst(j,r)

*	IMPORT WITH TRADE COST
$prod:m(i,r)$m0(i,r)   s:esubmm(i,r)  s.tl:0
        o:pm(i,r)	q:m0(i,r)
        i:p(i,s)	q:vxmd(i,s,r)   p:pvxmd(i,s,r) s.tl: 
+			a:irs(s) t:(-tx(i,s,r)) a:irs(r) t:(tm(i,s,r)*(1-tx(i,s,r)))
        i:pt(j)#(s)	q:vtwr(j,i,s,r) p:pvtwr(i,s,r) s.tl: a:irs(r) t:tm(i,s,r)

*	INVESTMENT PRODUCTION:
$prod:inv(r)
	o:pinv(r)		q:inv0(r)	
	i:pa(i,r)$net(i)	q:id0(i,"i",r)		p:pa0(i,"i",r)	a:irs(r) t:ta(i,"i",r)
        i:pe(i,"i",r)$elec(i)	q:id0("etd","i",r)	p:pa0("etd","i",r)	
	i:pe(i,"i",r)$enoe(i)	q:id0(i,"i",r)		p:pa0(i,"i",r)	a:irs(r) t:ta(i,"i",r)

*	GOVERNMENT CONSUMPTION:
$prod:gov(r)	s:delas("g",r)
        o:pgov(r)	q:gov0(r)
        i:pa(i,r)$net(i)	q:id0(i,"g",r)		p:pa0(i,"g",r)	a:irs(r) t:ta(i,"g",r)
        i:pe(i,"g",r)$elec(i)	q:id0("etd","g",r)	p:pa0("etd","g",r)
        i:pe(i,"g",r)$enoe(i)	q:id0(i,"g",r)		p:pa0(i,"g",r)	a:irs(r) t:ta(i,"g",r)

*	PRIVATE CONSUMPTION:
$prod:c(r)	s:delas("c",r)  a:d_elas(r)  e:esube("c",r)  ele(e):0  ef(e):0
	o:pc(r)			q:c0(r)
        i:pa(i,r)$net(i)	q:id0(i,"c",r)		p:pa0(i,"c",r)		a:	a:irs(r) t:ta(i,"c",r)
        i:pe(i,"c",r)$elec(i)	q:id0("etd","c",r)	p:pa0("etd","c",r)	ele:
        i:pe(i,"c",r)$enoe(i)	q:id0(i,"c",r)		p:pa0(i,"c",r)		ef:	a:irs(r) t:ta(i,"c",r)

*	WELFARE:
$prod:w(r)
	o:pw(r)		q:w0(r)
	i:pc(r)		q:c0(r)
	i:pinv(r)	q:inv0(r)

$prod:ksup(r)$ke0(r)  t:etrae("cap")
        o:pk(r)		q:ke0(r)
        i:pks(r)	q:ke0(r)

$prod:lsup(g,r)$ld0(g,r)  t:etrae("lab")
        o:pl(g,r)	q:ld0(g,r)
        i:pls(r)	q:ld0(g,r)

$demand:ra(r)
        d:pw(r)		q:w0(r)
	e:pks(r)	q:ke0(r)
	e:pls(r)	q:le0(r)
	e:pr(g,r)	q:rd0(g,r)
	e:pln(g,r)	q:lnd0(g,r)
        e:pc(rnum)	q:vb(r)
	e:pc(rnum)	q:(-trnsf(r))
        e:pcarbr(r)	q:carblim(r)

	e:pghg(ghg,r)$(ghglim(ghg,r)$(not wghgk))	q:ghglim(ghg,r)              		             			     
	e:pghgw(ghg)$(ghglim(ghg,r)$wghgk)		q:ghglim(ghg,r)				            			     
	e:pghgs(ghg,g,r)$(ghglimg(ghg,g,r))		q:ghglimg(ghg,g,r)			             			     

$demand:irs(r)
	d:ptax(r)		

$demand:govt(r)
	d:pgov(r)	q:gov0(r)
	e:ptax(r)	q:tax0(r)
	e:pc(rnum)	q:trnsf(r)

$report:

	v:kd(g,r)						i:pk(r)		prod:y(g,r)
	v:ld(g,r)						i:pl(g,r)	prod:y(g,r)
	v:rd(g,r)						i:pr(g,r)	prod:y(g,r)
	v:lnd(g,r)						i:pln(g,r)	prod:y(g,r)
	v:id(i,g,r)$(y0(g,r)$net(i))				i:pa(i,r)	prod:y(g,r)
	v:id(i,g,r)$(y0(g,r)$tnd(i))				i:pa(i,r)	prod:eled(g,r)
	v:id(i,g,r)$(y0(g,r)$elec(i))				i:pa(i,r)	prod:eled(g,r)
	v:id(i,g,r)$enoe(i)					i:pa(i,r)	prod:ed(i,g,r)

	v:id(i,g,r)$(id0(i,"c",r)$sameas(g,"c")$(net(i)))	i:pa(i,r)	prod:c(g,r)
	v:id(i,g,r)$(id0(i,"g",r)$sameas(g,"g")$(net(i)))	i:pa(i,r)	prod:gov(g,r)
	v:id(i,g,r)$(id0(i,"i",r)$sameas(g,"i")$(net(i)))	i:pa(i,r)	prod:inv(g,r)

	v:ad(i,r)		i:p(i,r)	prod:a(i,r)
	v:am(i,r)		i:pm(i,r)	prod:a(i,r)

	v:trade(i,s,r)		i:p(i,s)	prod:m(i,r)
	v:vstd(j,r)		i:p(j,r)	prod:yt(j)

	v:eletnd(g,r)$id0("etd",g,r)				i:pe("ele",g,r)	prod:y(g,r)

$offtext
$sysinclude mpsgeset core

pcarbr.l(r) = 0;
pe.l("ele",g,r) = pa0("etd",g,r);

*	Benchmark replication

core.workspace = 256;
core.iterlim = 0;
core.optfile = 1;
$include core.gen
solve core using mcp;

