
#
#  GAP functions for MathBook proofs.
#
#By "Scott H. Murray" <smurray@win.tue.nl> and Arjeh Cohen
#<A.M.Cohen@tue.nl>

#last edit amc Sep 22 2002

#Filename: permgps.g 
#Subject: elementary permutation group algorithms with certificates

#Introduction:
#This file with GAP functions is connected with our paper entitled
#    An automated proof theory approach to
#     computation with permutation groups

#The paper deals with the following eight queries
#name          GAP function             
#--------------------------------
#membership    IsIn_Proof        
#subgroup      IsSubgroup_Proof  
#orbit         Orbit_Proof       
#Schreier      SchreierData      
#stabiliser    IsStabiliser_Proof
#base          IsBase_Proof
#nonmember     IsNotIn_Proof
#order         Order_Proof

#important:
#For each of these functions there are query outputs
#furnished by the so-called Verbal version, eg VerbalIsIn_Proof.



#Word2Group (G,wd)
#gives the formal word multiplied out in the group G.
#the input word wd is given by an even length array;
#the odd place indicates the generator
#the even place the exponent of the generator indicated in the
#previous component.
#eg wd = [2,-1,3,2] leads to g_2^(-1)g_3^2 if G is generated by g_1,g_2,g_3.
Word2Group := function(G,wd)
local ret, i, gens;
  ret := One(G);
  gens := GeneratorsOfGroup(G);
  for i in [1..Length(wd)/2] do
        ret := ret * gens[wd[2*i-1]]^(wd[2*i]);
  od;
  return ret;
end;

#Fastwrd2wrd(w) translates a fast word in schreier vector convention
#to an ordinary word (as described for wd in the comment for
#Word2Group).
#The fast word w is given by an array
#of integers whose absolute values indicate the generator
#and whose sign indicate if the exponent is 1 or -1
#useful when applied to output of Backtrack
#eg [-2,3,3] leads to [2,-1,3,1,3,1].
Fastwrd2wrd := function(w)
local i, ans;
 ans := [];
 for i in w do
   if i > 0 then Append(ans,[i,1]);
   elif i<0 then Append(ans,[-i,-1]);
   fi;
 od;
  return ans;
end;

#FastWord2Group(G,wd) gives the formal word multiplied out in the group G.
#The input word wd is given by an array
#of integers whose absolute values indicate the generator
#and whose sign indicate if the exponent is 1 or -1.
#The is useful when applied to output of Backtrack.
FastWord2Group := function(G,wd)
  return Word2Group(G,Fastwrd2wrd(wd));
end;

#ModifyExps(G,wd) changes the exponents of a formal word wd
#to equivalent exponents wrt to the group G.
#It is a technical trick to get better exponents.
ModifyExps := function(G,wd) 
local gens, i, ans, x;
  ans := wd;
  gens := GeneratorsOfGroup(G);
  for i in [1..Length(ans)/2] do
        x := ans[2*i-1];
        ans[2*i] := ans[2*i] mod Order(gens[x]);
  od;
  return ans;
end;


#IsIn_Proof(g,G,Gletter) returns a word in the generators of G representing g.
#This shows that g is in G.
#The parameter Gletter is a string indicating how to represent the
#generators as words in a formal group.
IsIn_Proof := function(g, G , Gletter)
local F, hom, preim, wrd, x, i, gens;
  if not (g in G) then
     Error("the element does not belong to the group");
  fi;
  F := FreeGroup(Length(GeneratorsOfGroup(G)),LowercaseString(Gletter));
  hom := 
    GroupHomomorphismByImages(F,G,GeneratorsOfGroup(F),GeneratorsOfGroup(G));
  if Size(G) = 1 then return Word2Group(F,[]); fi;
  preim := PreImagesRepresentative(hom, g);
  if preim = fail then return fail; fi;
  wrd :=  Flat(ExtRepOfObj(preim));
  return Word2Group(F,ModifyExps(G,wrd));
end;

VerbalIsIn_Proof := function(g,G,gletter,Gletter)
  if not (g in G) then
     Print("Oops: the element does not belong to the group\n");
  else
    Print("Proof that ",gletter," = ",g," belongs to\n ",Gletter," = ",G,".\n");
    Print(" The element ", gletter," can be written as follows as a word in");
    Print(" the generators of ",Gletter,":\n"); 
    Print(" ",g,"=",IsIn_Proof(g, G, Gletter),",\n");
    Print(" and so belongs to ", Gletter,".");
    Print("\nQED(isin)\n\n");
  fi;
end;


# Returns words for the generators of H in the generators of G
IsSubgroup_Proof := function(H, G , letter) local h;
  return List(GeneratorsOfGroup(H), h -> IsIn_Proof(h, G, letter));
end;

VerbalIsSubgroup_Proof := function(H, G, Hletter, Gletter) 
local isp, i, hgens, l, h;
  hgens := GeneratorsOfGroup(H);  
  l := 0;
  for i in [1..Length(hgens)] do
    if not(hgens[i] in G) then l := i; fi;
  od;
  if l>0 then
    Print("Oops: the ",l,"-th generator of H is not in G\n");
  else
    isp := IsSubgroup_Proof(H, G ,Gletter);
    Print(" Proof that ",Hletter," = ",H," is a subgroup of\n ");
    Print(Gletter," = ",G,".\n");
    Print(" Clearly, it suffices to verify that\n");
    Print(" each of the generators of ",Hletter," belongs to ",Gletter,".\n");
    Print(" Below we give a word in the");
    Print(" generators of ",Gletter,"\n for");
    Print(" each generator of ",Hletter,".\n");
    l := Length(isp);
    for i in [1..l] do
      if (i mod 10)=1 and i<>11 then Print(" ",i,"-st"); 
      elif (i mod 10)=2 and i<>12 then Print(" ",i,"-nd");
      elif (i mod 10)=3 then Print(" ",i,"-rd");
      else Print(" ",i,"-th"); 
      fi;
      Print(" generator of ",Hletter," = ",hgens[i]," = ",isp[i]);
      if i<l then Print(",\n"); else Print("."); fi;
    od;
    Print("\n This establishes that each generator of ");
    Print(Hletter," belongs to ",Gletter,", and so ",Hletter);
    Print(" is a subgroup of ",Gletter,".");
    Print("\nQED(issubgroup)\n\n");
  fi;
end;


# Returns a Schreier data, consisting of 
# orbit, Schreier vector and back pointers.
SchreierData := function(G, i)
local bpnt, gens, orbit, svect, bpnts, pnt, j, im;
  bpnt := i;
  gens := GeneratorsOfGroup(G);
  orbit := [bpnt];  svect := [0];  bpnts := [0];
  for pnt in orbit do
    for j in [1..Length(gens)] do
      im := pnt^gens[j];
      if not im in orbit then
        Add(orbit, im);  Add(svect,j);  Add(bpnts, pnt);
      fi;
      im := pnt/gens[j];
      if not im in orbit then
        Add(orbit, im);  Add(svect, -j);  Add(bpnts, pnt);
      fi;
    od;
  od;
  return [orbit, svect, bpnts];
end;


# SchreierBacktrack returns the word in the generators that takes pnt to the
# base point i according to the Schreier data with base point i
#think of ss as SchreierData(G, i).
SchreierBacktrack := function(ss , pnt)
local btrecurse, orbit, svect, bpnts;
  orbit := ss[1];  svect := -ss[2];  bpnts := ss[3];
  btrecurse := function(pnt)
  local idx, ret;
    if pnt = orbit[1] then  return [];  fi;
    idx := Position(orbit, pnt);
    ret := btrecurse(bpnts[idx]);  Add(ret, svect[idx]);
    return ret;
  end;
  return Reversed(btrecurse(pnt));
end;

# Backtrack returns a word in the generators that takes pnt to the
# base point i.
Backtrack := function(G, i, pnt)
  return SchreierBacktrack(SchreierData(G, i), pnt);
end;

Schreiert := function(ss,pnt,G,Gletter)
local F, hlp;
  F := FreeGroup(Length(GeneratorsOfGroup(G)),LowercaseString(Gletter));
  hlp := Fastwrd2wrd(SchreierBacktrack(ss,pnt));
  return [Inverse(Word2Group(F,hlp)), Inverse(Word2Group(G,hlp))];
end;

Orbit_Proof := function(G,x,Gletter) local ans, orbit, i, F, hlp;
  orbit := Orbit(G,x);
  ans := [];
  F := FreeGroup(Length(GeneratorsOfGroup(G)),LowercaseString(Gletter));
  for i in orbit do 
    hlp := ModifyExps(G,Fastwrd2wrd(Backtrack(G,i,x)));
    Add(ans, [i,Word2Group(F,hlp),Word2Group(G,hlp)]); 
  od;
  Sort(ans);
  return ans;
end;


VerbalOrbit_Proof := function(G,x,Gletter) 
local orbit, i, orbpf, orbe, orbd, l;
  orbpf := Orbit_Proof(G,x,Gletter);
  Print("The orbit of x = ",x," under\n");
  Print(" G = ",G,"\n");
  l := Length(orbpf);
  orbit := orbpf{[1..l]}[1];
  orbe := orbpf{[1..l]}[2];
  orbd := orbpf{[1..l]}[3];
  Print(" is X = ",orbit,".\n");
  Print("Proof: ");
  Print(" By the straightforward check that the cycles containing\n");
  Print(" points of X do not contain any points not in X,\n");
  Print(" it follows that each generator of ",Gletter," leaves X invariant.\n");
  Print(" It remains to show that the points in X are images of");
  Print(" x under elements of G.\n");
  Print(" This is done in the lines below.\n");
  for i in [1..Length(orbit)] do
    Print("  ", orbit[i]," = ", x ," ", orbd[i]," = ", x ," ", orbe[i],"\n");
  od;
  Print("QED(orbit)\n\n");
end;

VerbalSchreierData := function(G, x , Gletter)
local ss, orbit, svect, backpnt, j, gens;
  ss := SchreierData(G, x);
  Print("The Schreier data for\n ", Gletter," = ",G,"\n");
  Print(" at x = ",x," is the table \n");
  for j in [1..3] do Print(" ",ss[j],"\n"); od;
  gens := GeneratorsOfGroup(G);
  orbit := ss[1];  svect := ss[2];  backpnt := ss[3];
  Print(" It has three rows, the first of which represents the");
  Print(" ",Gletter,"-orbit X of ",x,".\n");
  VerbalOrbit_Proof(G,x,Gletter);
  Print("To show that the second row is a Schreier vector\n");
  Print(" and the third row are its backpointers,\n");
  Print(" we consider the each column x,v,z of the table and verify\n"); 
  Print(" that the image of x under the (-v)-th generator");
  Print(" of ",Gletter," equals z\n");
  Print(" if v<0, and that the image of x under the inverse of the\n");
  Print(" v-the generator equals z if v>0.\n"); 
  for j in [1..Length(orbit)] do
    Print(orbit[j], "\t", FastWord2Group(G,[-svect[j]]), "\t= ", backpnt[j], "\n");
  od;
  Print("QED(schreierdata)\n\n");
end;

# Input:  a group G, a vertex x and a group H
#         which should be the stabilizer in G of x
# Output: IsSubgroup_Proof(H,G) together with a proof that every
#         Schreier generator is in H
IsStabiliser_Proof := function(G, x ,H,Gletter,Hletter)
local ss, orbit, backtracks, s, i, j, wd, gens;
  ss := SchreierData(G, x);  orbit := ss[1];
  backtracks := List(orbit, pnt -> SchreierBacktrack(ss,pnt));
  gens := GeneratorsOfGroup(G);
  s := [];
  for i in [1..Length(orbit)] do
    for j in [1..Length(gens)] do
      wd := Reversed(-backtracks[i]);
      Add(wd, j);
      wd := Concatenation(wd, backtracks[Position(orbit,orbit[i]^(gens[j]))]);
      Add(s, [orbit[i],j,wd,IsIn_Proof(FastWord2Group(G,wd),H,Hletter)]);
    od;
  od;
  return [IsSubgroup_Proof(H,G,Gletter), s, ss, backtracks];
end;

VerbalIsStabiliser_Proof := function(G, x ,H, Gletter, Hletter)
local i,j,k, isp, wd, F, gens, orbit, hlp, tees, Fgens;
  if not (Stabilizer(G,x) = H) then
     Print("Oops: ",Hletter," is not the stabilizer in", Gletter," of ",x,"\n");
  else
    isp := IsStabiliser_Proof(G,x,H,Gletter,Hletter);
    F := FreeGroup(Length(GeneratorsOfGroup(G)),LowercaseString(Gletter));
    Fgens := GeneratorsOfGroup(F);
    Print(" The stabilizer in ",Gletter," = ",G,"\n");
    Print(" of x = ",x," is ",Hletter," = ",H,".\n");
    Print("Proof:\n");
    Print(" First we check that ",Hletter," is a subgroup of ",Gletter,": \n");

    VerbalIsSubgroup_Proof(H,G,Hletter,Gletter);

    Print("Next we determine the Schreier data for ",Gletter," at x:\n");
    VerbalSchreierData(G,x,Gletter);

    Print("From the Schreier data we find that the Schreier elements\n");
    Print(" are as follows:\n");
    orbit := isp[3][1];
    tees := [];
    for j in [1..Length(orbit)] do
      hlp := FastWord2Group(F,isp[4][j]);
      tees[orbit[j]] := hlp;
      Print(" t(", orbit[j], ") = \t", hlp ,"\t = ");
      Print(FastWord2Group(G,isp[4][j]), "\n");
    od;
    Print("\n");
    Print("Finally we check that each Schreier generator is in ",Hletter,":\n");
    for  k in isp[2] do
        Print("  Schreier gen(",k[1],",",k[2],") \t= ");
        hlp := (tees[k[1]])*Fgens[k[2]];
        Print(" ",hlp," t(",hlp,")^(-1) \t=");
        Print(" ",FastWord2Group(F,k[3]),"\t= ",k[4],"\n");
    od;
    Print(" By Schreier's lemma, we conclude that ",Hletter," is\n");
    Print(" the stabilizer in ",Gletter," of x.\nQED(stabiliser)\n\n");
  fi;
end;


#[-j] cat backtracks[i] : orbit[i]^G.j -> orbit[i] -> bpnt
#Reverse([-c:c in backtracks[i]]) cat [j] : bpnt -> orbit[i] ->  orbit[i]^G.j
#backtracks[Position(orbit,orbit[i]^G.j)] : orbit[i]^G.j -> bpnt


# amc version:
# Input a group G and a sequence B of G (for instance B = BaseOfGroup(G)).
# Returns a sequence of quadruples of:
# Generators for the stabilizer K_{i} in K_{i-1} of B[1],...B[i], and 
# a proof that K_{i} is a subgroup of B[1],...B[i].
# a proof that K_{i} is the stabiliser in K_{i-1} of B[1],...B[i].
# the K_{i-1} orbit of B[i]
# indexed by i=1,...Length(B)
#the last component of hte output is the base again.
IsBase_Proof := function(G , B)
local H,K, s, b;
  s := [];
  H := G;
  for b in B do
    K := Stabilizer(H,b);
    Add(s, [GeneratorsOfGroup(K),
             IsStabiliser_Proof(H,b,K,"H","K"),
             Orbit(H,b)]);
    H := K;
  od;
  Add(s,B);
  return s;
end;

#truncatej =0 gives usual base proof
#j := truncatej>0 allows you to stop with an arbitrary sequence
#at step j. This is useful for IsNotIn_Proof

VerbalIsBase_Proof := function(G , B, Gletter, truncatej)
local ibp, hlp, i,j,k,l, isp, H, F, ii, ll, Hgens, hgens, K;
  ibp := IsBase_Proof(G,B);
  l := Length(B);
  if truncatej = 0 then
    Print("We show that B = ",B," is a base for  ",Gletter," = ",G,"\n");
    Print(" We do this by providing,\n");
   else
    Print(" We provide,\n");
    l := truncatej;
  fi;
  Print(" for i = 1, .. ,",l," a sequence of");
  Print(" elements A[i] of  ",Gletter," such that,\n");
  Print(" writing H[i] for the subgroup of G generated by A[i],\n");
  Print(" H[i] is the stabilizer of B[1],..,B[i] in G.\n");
  Print(" To this end, we list, for each i,\n");
  Print("  1) the sequence A[i],\n");
  Print("  2) a proof that H[i] is the stabilizer of B[i] in H[i-1],\n");
  Print("  3) the determination of the H[i-1]-orbit of B[i].\n");
  H := G;
  for i in [1..l] do
     Print("Case i = ",i,". We write H = H[",i-1,"] and K = H[",i,"].\n");
     hlp := ibp[i];
     Print("  A[",i,"] = ",hlp[1],"\n");
     isp := hlp[2];
     F := FreeGroup(Length(GeneratorsOfGroup(H)),"h" );
     K := Subgroup(G,hlp[1]);
     VerbalIsStabiliser_Proof(H, B[i] ,K, "H","K");
     H := K;
     Print("\n");  
  od;
  Print(" Having checked that each <A[i]> is the stabilizer");
  Print(" of B[i] in <A[i-1]>,\n");
  if truncatej = 0 then
    Print(" and that <A[",l,"]> is the trivial group,\n");
  fi;
  Print(" we conclude that ",B,"\n");
  if truncatej = 0 then
    Print(" is a base");
  else
    Print(" is a sequence");
  fi;
  Print(" with stabilizer chain the groups <A[i]>\n");
Print(" for i = 1,...,",l,".\n");
Print("QED(base)\n\n");
end;


# Input a permutation g and a group G to which g does not belong
# Returns an array a such that B = a[1] is a sequence of vertices,
# h=a[2] is an element h of G such that g*h 
# fixes each element of B, 
# a[3] is a proof that h is in G
# a[4] are generators of the stabilizer H in G of the vertices in the
# sequence B, and 
# a[5] is a proof that H is this stabilizer (part of IsBase)
# x = a[6] is a vertex such that x^(g*h) is not in the
# H-orbit of x
# a[7] is the H-orbit of x and 
# a[8] is a proof of this fact.

IsNotIn_Proof := function(hh, G ,letter)
local B, isbase, i, jj, kk, hf, hlp, H, horb, orbpf, orbpe;
  B := BaseOfGroup(G);
  isbase := IsBase_Proof(G,B);
  i := 1;
  H := G;
  kk := hh;
  while i <= Length(B) and B[i]^kk in Orbit(H,B[i]) do
     hlp := FastWord2Group(H,Backtrack(H,B[i],B[i]^kk));
     kk := kk * hlp;
     H := Stabilizer(H,B[i]);
#     Print("kk fixes B[i]? ",kk," fixes " ,B[i],"\n");
     i := i + 1;
#     Print("inISNOTIN i,kk  = ",i,kk,"\n");
  od;
  hlp := hh^(-1)*kk;
  orbpf := Orbit_Proof(H,B[i],"H");
#  horb := orbpf[1];
#  orbpf := orbpf[2];
   horb := [];
   orbpe := [];
   for jj in orbpf do 
      Add(horb,jj[1]);
      Add(orbpe,jj[2]);
   od;
   return [ B{[1..i-1]}, hlp, IsIn_Proof(hlp,G,letter), GeneratorsOfGroup(H),
           isbase{[1..i-1]}, B[i], horb, orbpe  ];
end;


VerbalIsNotIn_Proof := function(g,G,gletter,Gletter)
  local INSPdata, B, h, x, i, horb, orbpf, hletter;
  if g in G then 
     Print("Hmmmm.... actually,", gletter," is in  ",Gletter,"\n");
  else
    Print(gletter," = ",g," is not in ",Gletter," = ",G,",\n");
    Print(" Proof:\n\n");
    INSPdata :=  IsNotIn_Proof(g, G,"G");
    B := INSPdata[1];
    Print(" Consider the following list of points:\n");
    Print(" B = ",B,"\n");
    h := INSPdata[2];
    if gletter <> "h" then hletter := "h"; else hletter := "k"; fi;
    Print(" Let ",hletter," = ",h,". ");
    Print(" Then ",hletter," in  ",Gletter,",\n");
    Print(" as it is the following word in the generators: ");
    Print(hletter," = ",INSPdata[3],".\n");
    Print(" Moreover, ",gletter," ",hletter," =", g*h," fixes each element of B.\n");
    Print(" The stabilizer in ",Gletter," of all points of B is the group \n");
    Print(" H = <",INSPdata[4],">.\n");
    Print(" To show this, we proceed as follows.\n");
    VerbalIsBase_Proof(G,B,"G",Length(B));
    x := INSPdata[6];
    Print(" Now consider the point x = ", x,".\n");
    Print(" Its image under g h is the point x g h  = ",
    x," ",(g*h)," = ", x^(g*h),".\n");
    Print(" It does not belong to the H-orbit X of x, which is\n");
    horb :=  INSPdata[7];
    Print(" ",horb,".\n");
    orbpf :=  INSPdata[8];
    Print("Here is a proof that X is the H-orbit of x :\n");
    for i in [1..Length(horb)] do
        Print(" ", orbpf[i]," = ",orbpf[i]," maps ", x, " to ");
        Print(horb[i],"\n");
    od;
    Print(" Therefore, g h does not belong to  ",Gletter,".\n");
    Print(" As h belongs to  ",Gletter,",\n");
    Print(" this implies that g is not in  ",Gletter,".\nQED(notin).\n\n");
  fi;
end;



IsNotSubgroup_Proof := function(H, G ,Gletter)
local gens, i;
  gens := GeneratorsOfGroup(H);
  i := 0;
  repeat  i := i + 1;
  until not gens[i] in G;
  return [ i, IsNotIn_Proof(gens[i], G ,Gletter) ];
end;


#returns a triple consisting of the order,
#the orbits sizes of the stabilizers in the chain
#and the stabilizer chain with proof
Order_Proof := function(G)
local hlp, B, isbase, orbss, i;
  B := BaseOfGroup(G);
  isbase := IsBase_Proof(G,B);
  orbss := [];
  for i in [1..Length(isbase)-1] do
     hlp := Length(isbase[i][3]);
     Add(orbss,hlp);
  od;
  return [Product(orbss),orbss,isbase,B];
end;




VerbalOrder_Proof := function(G)
local op, hlp, i, B, szg, H,K;
  szg := Size(G);
  op := Order_Proof(G);
  B := op[4];
  Print(" Proof that the order of G is ", szg,":\n");
  Print(" B = ",B," is a base of G.\n");
  VerbalIsBase_Proof(G,B,"G",0);
#   Print(op[3],"\n");
  Print(" For i = 1...",Length(B)," the <A_[i-1]>-orbit of B[i] and its size is\n");
  for i in [1..Length(B)] do
     Print("  ", i,": ",op[3][i][3]," of size ",op[2][i],"\n");
# G_i orbit of B[i] = op[3][i][3]
  od;
  Print(" The product of the orbit sizes is ", op[1],".\n");
  Print(" By the stabilizer chain properties,\n");
  Print(" this is the order of G.\n");
  Print("QED(order)\n\n");
end;



# Test code
#G := Group((1,2,3,4)(5,7,9,11)(6,8,10,12), (2,4)(7,12)(8,11), (5,6));
#g := Random(G); 
#repeat  h := Random(SymmetricGroup(12));  until not h in G;

#IsIn_Proof(g, G,"G");
#IsSubgroup_Proof(Group(g,h), G,"G");
#SchreierData(G, 1); SchreierData(G, 3);
#Backtrack(G, 1, 7);
#H := Stabilizer(G,1);
#IsStabiliser_Proof(G, 1 , H,"G","H");
#IsNotIn_Proof(x, G ,"g");
#IsNotSubgroup_Proof(Group(g,x), G);
#VerbalOrder_Proof(G);

# Aut(Gamma), Gamma a graph










