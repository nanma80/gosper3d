#!/usr/bin/env wolframscript

normalizedInnerProd[u_, v_] := u.v/Norm[u]/Norm[v];
getLattice[vectors_, limit_] := Flatten[
    Table[
        {i, j, k}.vectors, {i, -limit, limit}, {j, -limit, limit}, {k, 0, 0}
    ], 2];
getClosest[v_, centers_] := Module[
    {minDistance, output},
    minDistance = Min[Map[Norm[v - #]&, Select[centers, Norm[#-v]>0&]]];
    Select[centers, Norm[# - v] == minDistance&]
];

origin = {0, 0, 0};
axis = {1, 1, 1};

baseVectors = {
    {-1, 1, 0},
    {0, -1, 1},
    {1, 0, -1}
};


limit = 3;

baseLattice = getLattice[baseVectors, limit];

(* Select[baseLattice, 
    normalizedInnerProd[#, axis] == Sqrt[2/3]&
]
 *)
(* 
{{4, 1, 1}, {1, 1, 0}, {2, 2, 0}, {1, 0, 1}, {2, 0, 2}, {0, 1, 1}, {1, 4, 1}, {0, 2, 2}, {1, 1, 4}}
 *)

newVectors = {
    {-2, 1, 0},
    {0, -2, 1},
    {1, 0, -2}
}.baseVectors;

(* newVectors = 4 * baseVectors; *)
newLattice = getLattice[newVectors, limit];

baseRegion = {};
ambiguousRegion = {};

For[i=1, i<=Length[baseLattice], i++,
    baseLatticePoint = baseLattice[[i]];
    minDistance = Min[Map[Norm[baseLatticePoint - #]&, newLattice]];
    originDistance = Norm[baseLatticePoint - origin];
    If[originDistance == minDistance,
        closestCenters = Select[newLattice, Norm[# - baseLatticePoint] == minDistance&];
        If[Length[closestCenters] == 1,
            AppendTo[baseRegion, baseLatticePoint],
            AppendTo[ambiguousRegion, baseLatticePoint]
        ];
    ];
];

Print[Length[baseRegion]];
Print[Length[ambiguousRegion]];

Print[baseRegion];
Print[ambiguousRegion];

