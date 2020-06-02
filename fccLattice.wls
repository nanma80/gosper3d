#!/usr/bin/env wolframscript

normalizedInnerProd[u_, v_] := u.v/Norm[u]/Norm[v];
getLattice[vectors_, limit_] := Flatten[
    Table[
        {i, j, k}.vectors, {i, -limit, limit}, {j, -limit, limit}, {k, -limit, limit}
    ], 2];

origin = {0, 0, 0};
axis = {1, 1, 1};

baseVectors = {
    {0, 1, 1},
    {1, 0, 1},
    {1, 1, 0}
};

limit = 4;

baseLattice = getLattice[baseVectors, limit];

Select[baseLattice, 
    normalizedInnerProd[#, axis] == Sqrt[2/3]&
]

(* offset = {-1, -1, -1}; *)
offset = {0, 0, 0};

baseLattice = Map[# + offset &, baseLattice];

newVectors = {
    {4, 1, 1},
    {1, 4, 1},
    {1, 1, 4}
};
19/18

(* 
newVectors = {
    {1, 4, 9},
    {9, 1, 4},
    {4, 9, 1}
};
333/24
 *)
(* newVectors = 3 * baseVectors; *)
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

(* Print[baseRegion];
Print[ambiguousRegion];
 *)