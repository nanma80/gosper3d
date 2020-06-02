#!/usr/bin/env wolframscript

skipBaseRegionGeneration = True;

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

limit = 10;

baseLattice = getLattice[baseVectors, limit];

SortBy[baseLattice, N[Norm[#]]&][[Range[20]]]

newVectors = 3 * baseVectors;
newLattice = getLattice[newVectors, limit];

SortBy[newLattice, N[Norm[#]]&][[Range[20]]]
(* 000, 333, 600, 660 *)

If[skipBaseRegionGeneration,
    baseRegion = {{-2, 0, -2}, {0, -2, -2}, {0, 0, -2}, {0, 2, -2}, {2, 0, -2}, {-1, -1, -1}, {-1, 1, -1}, {1, -1, -1}, {1, 1, -1}, {-2, -2, 0}, {-2, 0, 0}, {-2, 2, 0}, {0, -2, 0}, {0, 0, 0}, {0, 2, 0}, {2, -2, 0}, {2, 0, 0}, {2, 2, 0}, {-1, -1, 1}, {-1, 1, 1}, {1, -1, 1}, {1, 1, 1}, {-2, 0, 2}, {0, -2, 2}, {0, 0, 2}, {0, 2, 2}, {2, 0, 2}},
    (* 000, 111, 220, 200, all perms and sign changes. 27 in total *)

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
    (* 27 *)
    Print[Length[ambiguousRegion]];
    (* 0 *)

    Print[baseRegion];
    Print[ambiguousRegion];
];

points1 = baseRegion;

centers2 = baseRegion * 3;

points2 = {};
For[centerIndex = 1, centerIndex <= Length[centers2], centerIndex++,
    points2 = Join[points2, offset[points1, centers2[[centerIndex]]]];
];
Print[Length[points2]];
Print[Length[Union[points2]]];

centers3 = baseRegion * 3^2;
points3 = {};
For[centerIndex = 1, centerIndex <= Length[centers3], centerIndex++,
    points3 = Join[points3, offset[points2, centers3[[centerIndex]]]];
];
Print[Length[points3]];
Print[Length[Union[points3]]];
(* no overlap *)

centers4 = baseRegion * 3^3;
points4 = {};
For[centerIndex = 1, centerIndex <= Length[centers4], centerIndex++,
    points4 = Join[points4, offset[points3, centers4[[centerIndex]]]];
];
Print[Length[points4]];
Print[Length[Union[points4]]];
(* no overlap *)

baseLattice//Length
(* 2197 *)
Complement[baseLattice, points4]//Length
(* 0. no gap *)


(* mogrify -crop 1164x1164+493+0 *.png *)