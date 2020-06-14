#!/usr/bin/env wolframscript

skipBaseRegionGeneration = True;
altMode = True;

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

r = 3;
s = Norm[{0, -r, r}- {r, 0, 0}];
pyramidpx = Pyramid[{{0, r, r}, {0, r, -r}, {0, -r, -r}, {0, -r, r}, {r, 0, 0}}];
pyramidnx = Pyramid[{{0, r, r}, {0, r, -r}, {0, -r, -r}, {0, -r, r}, {-r, 0, 0}}];

pyramidpy = Pyramid[{{r, 0, r}, {r, 0, -r}, {-r, 0, -r}, {-r, 0, r}, {0, r, 0}}];
pyramidny = Pyramid[{{r, 0, r}, {r, 0, -r}, {-r, 0, -r}, {-r, 0, r}, {0, -r, 0}}];

pyramidpz = Pyramid[{{r, r, 0}, {r, -r, 0}, {-r, -r, 0}, {-r, r, 0}, {0, 0, r}}];
pyramidnz = Pyramid[{{r, r, 0}, {r, -r, 0}, {-r, -r, 0}, {-r, r, 0}, {0, 0, -r}}];

srd = RegionUnion[pyramidpx, pyramidnx, pyramidpy, pyramidny, pyramidpz, pyramidnz];
srd//Volume




newVectors = If[altMode, 
    {{-5, 1, 1}, {1, -5, 1}, {1, 1, -5}},
    3 * baseVectors
];

newLattice = getLattice[newVectors, limit];

baseRegion = {{-2, 0, -2}, {0, -2, -2}, {0, 0, -2}, {0, 2, -2}, {2, 0, -2}, {-1, -1, -1}, {-1, 1, -1}, {1, -1, -1}, {1, 1, -1}, {-2, -2, 0}, {-2, 0, 0}, {-2, 2, 0}, {0, -2, 0}, {0, 0, 0}, {0, 2, 0}, {2, -2, 0}, {2, 0, 0}, {2, 2, 0}, {-1, -1, 1}, {-1, 1, 1}, {1, -1, 1}, {1, 1, 1}, {-2, 0, 2}, {0, -2, 2}, {0, 0, 2}, {0, 2, 2}, {2, 0, 2}};

scalingMatrixA = IdentityMatrix[3] * 3;
scalingMatrixB = RotationMatrix[Pi/3, {1, 1, 1}] * 3;

(* 
alt:
{{0, 0, 0}, {-5, 1, 1}, {-3, -3, -3}, {-1, -1, 5}, {-1, 5, -1}, {1, -5, 1}, {1, 1, -5}, {3, 3, 3}, {5, -1, -1}, {-4,
 -4, 2}, {-4, 2, -4}, {-2, 4, 4}, {2, -4, -4}, {4, -2, 4}, {4, 4, -2}, {-8, -2, -2}, {-6, 0, 6}, {-6, 6, 0}, {-2, -8,
 -2}, {-2, -2, -8}, {0, -6, 6}, {0, 6, -6}, {2, 2, 8}, {2, 8, 2}, {6, -6, 0}, {6, 0, -6}, {8, 2, 2}}

 *)


centerNamesList = {
    {1, 1, 1}, {1, 1, 2}, {1, 2, 1}, {1, 2, 2},
    {2, 1, 1}, {2, 1, 2}, {2, 2, 1}, {2, 2, 2}
};

(* 
centerNamesList = {
    {1, 1, 1, 1}, {1, 1, 1, 2}, {1, 1, 2, 1}, {1, 1, 2, 2},
    {1, 2, 1, 1}, {1, 2, 1, 2}, {1, 2, 2, 1}, {1, 2, 2, 2},
    {2, 1, 1, 1}, {2, 1, 1, 2}, {2, 1, 2, 1}, {2, 1, 2, 2},
    {2, 2, 1, 1}, {2, 2, 1, 2}, {2, 2, 2, 1}, {2, 2, 2, 2}
};
 *)

For[nameIndex = 1, nameIndex <= Length[centerNamesList], nameIndex++,
    centerNames = centerNamesList[[nameIndex]];
    Print[];
    Print[centerNames];
    centerSequence = {};
    pointsi = baseRegion;
    centers = baseRegion;

    For[i=1, i<=Length[centerNames], i++,
        centers = centers.
            If[centerNames[[i]] == 1,
                scalingMatrixA,
                scalingMatrixB
            ];

        points2 = {};
        For[centerIndex = 1, centerIndex <= Length[centers], centerIndex++,
            points2 = Join[points2, offset[pointsi, centers[[centerIndex]]]];
        ];

        If[Length[points2] == Length[Union[points2]] && Length[points2] == 27^(i+1),
            Print["no overlap"],
    
            Print["overlapping"];
            Print[Length[points2]];
            Print[Length[Union[points2]]];
            Print[27^(i+1)];
        ];

        pointsi = points2;
    ];

    If[Length[Complement[baseLattice, pointsi]] == 0,
        Print["no gap"],
        Print["there is gap or the generated points do not cover all baseLattice. The number of missing points are:"];
        Print[Length[Complement[baseLattice, pointsi]]];
    ];
];
