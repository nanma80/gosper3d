#!/usr/bin/env wolframscript

skipBaseRegionGeneration = True;
altMode = False;

normalizedInnerProd[u_, v_] := u.v/Norm[u]/Norm[v];
getLattice[vectors_, limit_] := Flatten[
    Table[
        {i, j, k}.vectors, {i, -limit, limit}, {j, -limit, limit}, {k, -limit, limit}
    ], 2];
offset[vectors_, offset_] := Map[ (# + offset) &, vectors];

origin = {0, 0, 0};
axis = {1, 1, 1};

baseVectors = {
    {1, 1, 1},
    {1, 1, -1},
    {1, -1, 1}
};

limit = 6;

baseLattice = getLattice[baseVectors, limit];

SortBy[baseLattice, N[Norm[#]]&][[Range[20]]]

newVectors = If[altMode, 
    {{-5, 1, 1}, {1, -5, 1}, {1, 1, -5}},
    3 * baseVectors
];

newLattice = getLattice[newVectors, limit];


baseRegion = {{-2, 0, -2}, {0, -2, -2}, {0, 0, -2}, {0, 2, -2}, {2, 0, -2}, {-1, -1, -1}, {-1, 1, -1}, {1, -1, -1}, {1, 1, -1}, {-2, -2, 0}, {-2, 0, 0}, {-2, 2, 0}, {0, -2, 0}, {0, 0, 0}, {0, 2, 0}, {2, -2, 0}, {2, 0, 0}, {2, 2, 0}, {-1, -1, 1}, {-1, 1, 1}, {1, -1, 1}, {1, 1, 1}, {-2, 0, 2}, {0, -2, 2}, {0, 0, 2}, {0, 2, 2}, {2, 0, 2}};


centers2a = baseRegion * 3;
(* 000, 333, 600, 660 *)
centers2b = SortBy[newLattice, N[Norm[#]]&][[Range[27]]];
(* 
alt:
{{0, 0, 0}, {-5, 1, 1}, {-3, -3, -3}, {-1, -1, 5}, {-1, 5, -1}, {1, -5, 1}, {1, 1, -5}, {3, 3, 3}, {5, -1, -1}, {-4,
 -4, 2}, {-4, 2, -4}, {-2, 4, 4}, {2, -4, -4}, {4, -2, 4}, {4, 4, -2}, {-8, -2, -2}, {-6, 0, 6}, {-6, 6, 0}, {-2, -8,
 -2}, {-2, -2, -8}, {0, -6, 6}, {0, 6, -6}, {2, 2, 8}, {2, 8, 2}, {6, -6, 0}, {6, 0, -6}, {8, 2, 2}}

 *)


centerNames = {2, 1, 2};
centerSequence = {};
pointsi = baseRegion;

For[i=1, i<=Length[centerNames], i++,
    centers =
        If[centerNames[[i]] == 1,
            centers2a,
            centers2b
        ] * 3^(i-1);

    points2 = {};
    For[centerIndex = 1, centerIndex <= Length[centers], centerIndex++,
        points2 = Join[points2, offset[pointsi, centers[[centerIndex]]]];
    ];
    Print[Length[points2]];
    Print[Length[Union[points2]]];
    Print[27^(i+1)];

    pointsi = points2;
];

Complement[baseLattice, points2]//Length
(* 0 means no gap *)

