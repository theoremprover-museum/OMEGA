
#
#  GAP functions for GraphTheory proofs.
#
#By "Martin Pollet" <pollet@ags.uni-sb.de> 

#last edit mp Jul 6 2004

#Filename: graphs.g
#Subject: interface functions for the GRAPE package


RequirePackage("grape");



EGraph := function(V,E)
        local eset;
        eset:= Set(E,i->Set(i));
        return Graph(Group(()),Set(V),OnPoints, function(x,y) return Set([x,y]) in eset; end);
end;


PIsomorphism := function(V1,E1,V2,E2)
        local iso,pointwise,i;
        pointwise:="";
        iso:=GraphIsomorphism(EGraph(V1,E1),EGraph(V2,E2));
if IsPerm(iso) then
        for i in V1 do
        pointwise:=Concatenation(pointwise, "(", String(i), " ", String(i^iso), ")");
        od;
        return Concatenation("(", pointwise, ")");
else
        return iso;
fi;
end;



