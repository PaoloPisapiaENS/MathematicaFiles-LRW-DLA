(* ::Package:: *)

(* pIntg2[loop number,graph number] = {Nickel index, eps-expansion (in G-scheme)} *)
(* CONVENTION: d = 4 - 2*eps. To convert to 4-\[Epsilon], one needs to eps->\[Epsilon]/2 
and since it is normalized wrt Banana=1/eps, one also needs an extra Normalization factor 2^(-Loops).
e.g. pIntg2[2,1]=-1/4/eps -> -1/4 (2/\[Epsilon]) 2^-2 = -1/8*)

(* 1 graphs with 2 loops: *)
pIntg2[2,1] = {"e111|e|", Series[-1/4/eps-5/8-27/16*eps+3/2*eps^2*Zeta[3]-153/32*eps^2+15/4*eps^3*Zeta[3]-891/64*eps^3+1/40*Pi^4*eps^3+O[eps]^4,{eps,0,3}]}

(* 1 graphs with 3 loops: *)
pIntg2[3,1] = {"e112|22|e|", Series[-1/3/eps^2-4/3/eps-16/3+16/3*eps*Zeta[3]-64/3*eps+64/3*eps^2*Zeta[3]-256/3*eps^2+4/45*Pi^4*eps^2+O[eps]^3,{eps,0,2}]}

(* 4 graphs with 4 loops: *)
pIntg2[4,1] = {"e112|23|33|e|", Series[-1/4/eps^3-3/2/eps^2-33/4/eps+10*Zeta[3]-175/4+113/2*eps*Zeta[3]-1825/8*eps+1/6*Pi^4*eps+O[eps]^2,{eps,0,1}]}
pIntg2[4,2] = {"e112|33|e33||", Series[-3/8/eps^3-33/16/eps^2-345/32/eps+45/4*Zeta[3]-3525/64+495/8*eps*Zeta[3]-35625/128*eps+3/16*Pi^4*eps+O[eps]^2,{eps,0,1}]}
pIntg2[4,3] = {"e112|e3|333||", Series[1/32/eps^2+5/16/eps+293/128+939/64*eps-13/8*eps*Zeta[3]+44677/512*eps^2-65/4*eps^2*Zeta[3]-13/480*Pi^4*eps^2+O[eps]^3,{eps,0,2}]}
pIntg2[4,4] = {"e123|e23|33||", Series[-1/8/eps^3-13/16/eps^2-141/32/eps+2*Zeta[3]-1393/64+13*eps*Zeta[3]-12997/128*eps+1/30*Pi^4*eps+O[eps]^2,{eps,0,1}]}

(* 11 graphs with 5 loops: *)
pIntg2[5,1] = {"e112|23|34|44|e|", Series[-2/15/eps^4-17/15/eps^3-122/15/eps^2+34/3*Zeta[3]/eps-817/15/eps+268/3*Zeta[3]-1058/3+17/90*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[5,2] = {"e112|23|44|e44||", Series[-7/30/eps^4-9/5/eps^3-367/30/eps^2+239/15*Zeta[3]/eps-1187/15/eps+576/5*Zeta[3]-15031/30+239/900*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[5,3] = {"e112|23|e4|444||", Series[1/30/eps^3+23/60/eps^2+77/24/eps+1127/48-7/3*Zeta[3]+76697/480*eps-161/6*eps*Zeta[3]-7/180*Pi^4*eps+O[eps]^2,{eps,0,1}]}
pIntg2[5,4] = {"e112|33|444|e4||", Series[1/20/eps^3+23/40/eps^2+393/80/eps+5931/160-47/10*Zeta[3]+83601/320*eps-1081/20*eps*Zeta[3]-47/600*Pi^4*eps+O[eps]^2,{eps,0,1}]}
pIntg2[5,5] = {"e112|33|e44|44||", Series[-2/5/eps^4-14/5/eps^3-18/eps^2+96/5*Zeta[3]/eps-558/5/eps+672/5*Zeta[3]-3402/5+8/25*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[5,6] = {"e112|34|334|4|e|", Series[-1/10/eps^4-13/15/eps^3-61/10/eps^2+29/5*Zeta[3]/eps-589/15/eps+676/15*Zeta[3]-7271/30+29/300*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[5,7] = {"e112|34|e34|44||", Series[-2/15/eps^4-16/15/eps^3-106/15/eps^2+68/15*Zeta[3]/eps-216/5/eps+544/15*Zeta[3]-3818/15+17/225*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[5,8] = {"e112|e3|344|44||", Series[1/30/eps^3+7/15/eps^2+137/30/eps+571/15-47/15*Zeta[3]+8681/30*eps-658/15*eps*Zeta[3]-47/900*Pi^4*eps+O[eps]^2,{eps,0,1}]}
pIntg2[5,9] = {"e123|234|34|4|e|", Series[-2/5*Zeta[3]/eps^2-14/5*Zeta[3]/eps+6/5*Zeta[5]-86/5*Zeta[3]-1/150*Pi^4/eps-7/150*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[5,10] = {"e123|e23|44|44||", Series[-1/15/eps^4-3/5/eps^3-61/15/eps^2-14/15*Zeta[3]/eps-349/15/eps-42/5*Zeta[3]-1717/15-7/450*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[5,11] = {"e123|e24|34|44||", Series[-1/30/eps^4-1/3/eps^3-73/30/eps^2-1/15*Zeta[3]/eps-223/15/eps-2/3*Zeta[3]-2357/30-1/900*Pi^4+O[eps]^1,{eps,0,0}]}

(* 50 graphs with 6 loops: *)
pIntg2[6,1] = {"e112|23|34|45|55|e|", Series[-1/18/eps^5-23/36/eps^4-52/9/eps^3+79/9*Zeta[3]/eps^2-142/3/eps^2+1691/18*Zeta[3]/eps-3320/9/eps+79/540*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,2] = {"e112|23|34|55|e55||", Series[-5/48/eps^5-317/288/eps^4-607/64/eps^3+173/12*Zeta[3]/eps^2-86833/1152/eps^2+10117/72*Zeta[3]/eps-1327879/2304/eps+173/720*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,3] = {"e112|23|34|e5|555||", Series[11/576/eps^4+19/72/eps^3+6001/2304/eps^2+5725/256/eps-671/288*Zeta[3]/eps+1647407/9216-1123/36*Zeta[3]-671/17280*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[6,4] = {"e112|23|44|455|5|e|", Series[-1/8/eps^5-61/48/eps^4-1027/96/eps^3+63/4*Zeta[3]/eps^2-5367/64/eps^2+3575/24*Zeta[3]/eps-81553/128/eps+21/80*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,5] = {"e112|23|44|555|e5||", Series[7/192/eps^4+47/96/eps^3+3701/768/eps^2+32059/768/eps-491/96*Zeta[3]/eps+1037971/3072-3235/48*Zeta[3]-491/5760*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[6,6] = {"e112|23|44|e55|55||", Series[-11/48/eps^5-205/96/eps^4-3287/192/eps^3+275/12*Zeta[3]/eps^2-49825/384/eps^2+4837/24*Zeta[3]/eps-737879/768/eps+55/144*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,7] = {"e112|23|45|445|5|e|", Series[-7/144/eps^5-161/288/eps^4-2867/576/eps^3+469/72*Zeta[3]/eps^2-5101/128/eps^2+9659/144*Zeta[3]/eps-697795/2304/eps+469/4320*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,8] = {"e112|23|45|e45|55||", Series[-11/144/eps^5-227/288/eps^4-1247/192/eps^3+473/72*Zeta[3]/eps^2-56603/1152/eps^2+9185/144*Zeta[3]/eps-827405/2304/eps+473/4320*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,9] = {"e112|23|e4|455|55||", Series[5/144/eps^4+155/288/eps^3+1115/192/eps^2+61495/1152/eps-145/36*Zeta[3]/eps+1033105/2304-4495/72*Zeta[3]-29/432*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[6,10] = {"e112|33|344|5|55|e|", Series[-7/36/eps^5-15/8/eps^4-2213/144/eps^3+409/18*Zeta[3]/eps^2-34139/288/eps^2+801/4*Zeta[3]/eps-171119/192/eps+409/1080*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,11] = {"e112|33|444|55|5|e|", Series[1/16/eps^4+13/16/eps^3+507/64/eps^2+2199/32/eps-37/4*Zeta[3]/eps+143313/256-481/4*Zeta[3]-37/240*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[6,12] = {"e112|33|445|45|5|e|", Series[-13/144/eps^5-275/288/eps^4-4649/576/eps^3+353/36*Zeta[3]/eps^2-23957/384/eps^2+6559/72*Zeta[3]/eps-1068745/2304/eps+353/2160*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,13] = {"e112|33|445|e5|55||", Series[1/18/eps^4+31/36/eps^3+227/24/eps^2+12863/144/eps-79/9*Zeta[3]/eps+223097/288-2449/18*Zeta[3]-79/540*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[6,14] = {"e112|33|e34|5|555||", Series[5/144/eps^4+65/144/eps^3+2455/576/eps^2+3385/96/eps-115/36*Zeta[3]/eps+627365/2304-1495/36*Zeta[3]-23/432*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[6,15] = {"e112|33|e44|55|55||", Series[-5/12/eps^5-85/24/eps^4-1295/48/eps^3+175/6*Zeta[3]/eps^2-18865/96/eps^2+2975/12*Zeta[3]/eps-269255/192/eps+35/72*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,16] = {"e112|33|e45|45|55||", Series[-5/36/eps^5-95/72/eps^4-165/16/eps^3+70/9*Zeta[3]/eps^2-21515/288/eps^2+665/9*Zeta[3]/eps-302045/576/eps+7/54*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,17] = {"e112|34|334|5|55|e|", Series[-5/72/eps^5-37/48/eps^4-1933/288/eps^3+305/36*Zeta[3]/eps^2-30475/576/eps^2+2021/24*Zeta[3]/eps-50997/128/eps+61/432*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,18] = {"e112|34|335|4|55|e|", Series[-5/72/eps^5-37/48/eps^4-1933/288/eps^3+151/18*Zeta[3]/eps^2-30475/576/eps^2+1007/12*Zeta[3]/eps-50997/128/eps+151/1080*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,19] = {"e112|34|335|5|e55||", Series[-7/72/eps^5-1/eps^4-1189/144/eps^3+313/36*Zeta[3]/eps^2-18019/288/eps^2+989/12*Zeta[3]/eps-29293/64/eps+313/2160*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,20] = {"e112|34|335|e|555||", Series[1/32/eps^4+5/12/eps^3+515/128/eps^2+4351/128/eps-57/16*Zeta[3]/eps+137181/512-136/3*Zeta[3]-19/320*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[6,21] = {"e112|34|345|45|5|e|", Series[-1/3*Zeta[3]/eps^3-37/12*Zeta[3]/eps^2+9/4*Zeta[5]/eps-583/24*Zeta[3]/eps-1/180*Pi^4/eps^2-37/720*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,22] = {"e112|34|345|e5|55||", Series[-1/36/eps^5-49/144/eps^4-877/288/eps^3+65/36*Zeta[3]/eps^2-4505/192/eps^2+671/36*Zeta[3]/eps-190781/1152/eps+13/432*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,23] = {"e112|34|355|45|e5||", Series[-1/36/eps^5-49/144/eps^4-877/288/eps^3+31/18*Zeta[3]/eps^2-4505/192/eps^2+1297/72*Zeta[3]/eps-190781/1152/eps+31/1080*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,24] = {"e112|34|355|e4|55||", Series[-1/18/eps^5-5/8/eps^4-763/144/eps^3+25/9*Zeta[3]/eps^2-11305/288/eps^2+25*Zeta[3]/eps-51637/192/eps+5/108*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,25] = {"e112|34|e33|5|555||", Series[5/144/eps^4+65/144/eps^3+2455/576/eps^2+3385/96/eps-115/36*Zeta[3]/eps+627365/2304-1495/36*Zeta[3]-23/432*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[6,26] = {"e112|34|e34|55|55||", Series[-5/72/eps^5-35/48/eps^4-1675/288/eps^3+5/9*Zeta[3]/eps^2-23365/576/eps^2+35/6*Zeta[3]/eps-100145/384/eps+1/108*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,27] = {"e112|34|e35|45|55||", Series[-5/144/eps^5-115/288/eps^4-1945/576/eps^3+25/36*Zeta[3]/eps^2-9445/384/eps^2+575/72*Zeta[3]/eps-376985/2304/eps+5/432*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,28] = {"e112|34|e55|445|5||", Series[-5/36/eps^5-95/72/eps^4-165/16/eps^3+70/9*Zeta[3]/eps^2-21515/288/eps^2+665/9*Zeta[3]/eps-302045/576/eps+7/54*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,29] = {"e112|e3|334|5|555||", Series[-1/384/eps^3-15/256/eps^2-107/128/eps+85/192*Zeta[3]-14765/1536+1275/128*eps*Zeta[3]-199109/2048*eps+17/2304*Pi^4*eps+O[eps]^2,{eps,0,1}]}
pIntg2[6,30] = {"e112|e3|344|55|55||", Series[1/32/eps^4+9/16/eps^3+881/128/eps^2+565/8/eps-37/8*Zeta[3]/eps+334941/512-333/4*Zeta[3]-37/480*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[6,31] = {"e112|e3|345|45|55||", Series[1/96/eps^4+19/96/eps^3+319/128/eps^2+9965/384/eps-67/48*Zeta[3]/eps+371537/1536-1273/48*Zeta[3]-67/2880*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[6,32] = {"e112|e3|444|555|5||", Series[-1/192/eps^3-35/384/eps^2-205/192/eps+65/96*Zeta[3]-8045/768+2275/192*eps*Zeta[3]-285827/3072*eps+13/1152*Pi^4*eps+O[eps]^2,{eps,0,1}]}
pIntg2[6,33] = {"e112|e3|445|455|5||", Series[1/48/eps^4+37/96/eps^3+923/192/eps^2+6407/128/eps-79/24*Zeta[3]/eps+120089/256-2909/48*Zeta[3]-79/1440*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[6,34] = {"e123|224|4|555|e5||", Series[1/48/eps^4+7/24/eps^3+563/192/eps^2+1643/64/eps-67/24*Zeta[3]/eps+160405/768-469/12*Zeta[3]-67/1440*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[6,35] = {"e123|224|5|445|5|e|", Series[-1/24/eps^5-23/48/eps^4-401/96/eps^3+49/12*Zeta[3]/eps^2-6215/192/eps^2+309/8*Zeta[3]/eps-91201/384/eps+49/720*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,36] = {"e123|234|45|45|5|e|", Series[-5/6*Zeta[5]/eps^2+1/6*Zeta[3]^2/eps-25/4*Zeta[5]/eps-5/2268*Pi^6/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,37] = {"e123|234|45|55|e5||", Series[-1/4*Zeta[3]/eps^3-19/8*Zeta[3]/eps^2+7/6*Zeta[5]/eps-281/16*Zeta[3]/eps-1/240*Pi^4/eps^2-19/480*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,38] = {"e123|245|45|445||e|", Series[-1/4*Zeta[3]/eps^3-19/8*Zeta[3]/eps^2+19/12*Zeta[5]/eps-281/16*Zeta[3]/eps-1/240*Pi^4/eps^2-19/480*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,39] = {"e123|e23|34|5|555||", Series[1/288/eps^4+1/18/eps^3+659/1152/eps^2+591/128/eps+35/144*Zeta[3]/eps+140077/4608+35/9*Zeta[3]+7/1728*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[6,40] = {"e123|e23|44|55|55||", Series[-1/24/eps^5-23/48/eps^4-373/96/eps^3-23/6*Zeta[3]/eps^2-4787/192/eps^2-529/12*Zeta[3]/eps-46261/384/eps-23/360*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,41] = {"e123|e23|45|45|55||", Series[-1/72/eps^5-25/144/eps^4-47/32/eps^3-53/36*Zeta[3]/eps^2-5569/576/eps^2-1325/72*Zeta[3]/eps-54967/1152/eps-53/2160*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,42] = {"e123|e24|33|5|555||", Series[1/64/eps^4+7/32/eps^3+547/256/eps^2+4545/256/eps-95/96*Zeta[3]/eps+137637/1024-665/48*Zeta[3]-19/1152*Pi^4+O[eps]^1,{eps,0,0}]}
pIntg2[6,43] = {"e123|e24|34|55|55||", Series[-1/72/eps^5-3/16/eps^4-497/288/eps^3-13/18*Zeta[3]/eps^2-7271/576/eps^2-39/4*Zeta[3]/eps-28427/384/eps-13/1080*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,44] = {"e123|e24|35|45|55||", Series[-1/144/eps^5-29/288/eps^4-563/576/eps^3-17/72*Zeta[3]/eps^2-2887/384/eps^2-529/144*Zeta[3]/eps-108163/2304/eps-17/4320*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,45] = {"e123|e24|55|445|5||", Series[-1/48/eps^5-25/96/eps^4-431/192/eps^3-13/8*Zeta[3]/eps^2-5881/384/eps^2-325/16*Zeta[3]/eps-62399/768/eps-13/480*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,46] = {"e123|e45|334|5|55||", Series[-1/18/eps^5-7/12/eps^4-169/36/eps^3+35/18*Zeta[3]/eps^2-2431/72/eps^2+245/12*Zeta[3]/eps-3697/16/eps+7/216*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,47] = {"e123|e45|344|55|5||", Series[-1/72/eps^5-3/16/eps^4-497/288/eps^3-13/18*Zeta[3]/eps^2-7271/576/eps^2-39/4*Zeta[3]/eps-28427/384/eps-13/1080*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
pIntg2[6,48] = {"e123|e45|345|45|5||", Series[-1/12*Zeta[3]/eps^3-23/24*Zeta[3]/eps^2+2/3*Zeta[5]/eps-389/48*Zeta[3]/eps+23/3*Zeta[5]-16/3*Zeta[3]^2-1761/32*Zeta[3]-1/720*Pi^4/eps^2-23/1440*Pi^4/eps-389/2880*Pi^4+1/504*Pi^6+O[eps]^1,{eps,0,0}]}
pIntg2[6,49] = {"e123|e45|444|555|||", Series[-1/192/eps^3-35/384/eps^2-205/192/eps+85/96*Zeta[3]-8045/768+2975/192*eps*Zeta[3]-285827/3072*eps+17/1152*Pi^4*eps+O[eps]^2,{eps,0,1}]}
pIntg2[6,50] = {"e123|e45|445|455|||", Series[-1/72/eps^5-3/16/eps^4-497/288/eps^3-23/36*Zeta[3]/eps^2-7271/576/eps^2-73/8*Zeta[3]/eps-28427/384/eps-23/2160*Pi^4/eps+O[eps]/eps^1,{eps,0,-1}]}
