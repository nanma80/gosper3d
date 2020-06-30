#!/usr/bin/env wolframscript

useOffset = True;

epsilon = 0.000001;

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

limit = 3;

baseLattice = getLattice[baseVectors, limit];

Select[baseLattice, 
    normalizedInnerProd[#, axis] == Sqrt[2/3]&
]

allCandidates = Select[baseLattice, Norm[#]>0&];

randomDirection = {Pi, E, 1};

centerBaseCandidates = {};

targetInnerProd = 1/2;

For[i = 1, i <= Length[allCandidates], i++,
    seed1 = allCandidates[[i]];
    If[seed1[[1]] <= seed1[[2]] <= seed1[[3]],
        seed2Candidates = Select[allCandidates,
            N[Abs[normalizedInnerProd[seed1, #] - targetInnerProd]] < epsilon
            && Dot[seed1, randomDirection] < Dot[#, randomDirection]
            && N[ Abs[Norm[seed1]- Norm[#]]] < epsilon &
            ];
        If[Length[seed2Candidates] > 0,
            For[j = 1, j<= Length[seed2Candidates], j++,
                seed2 = seed2Candidates[[j]];
                seed3Candidates = Select[seed2Candidates, 
                    N[Abs[normalizedInnerProd[seed2, #] - targetInnerProd]] < epsilon
                    && Dot[seed2, randomDirection] < Dot[#, randomDirection] &
                    ];

                If[Length[seed3Candidates] > 0,
                    For[k = 1, k<= Length[seed3Candidates], k++,
                        seed3 = seed3Candidates[[k]];
                        Print[{seed1, seed2, seed3}];

                        AppendTo[centerBaseCandidates, {seed1, seed2, seed3}];
                    ];
                ];
            ];
        ];
    ];
];

offset = If[useOffset, -1/2 * Total[baseVectors], {0, 0, 0}];
baseLattice = Map[# + offset &, baseLattice];

For[iBase = 1, iBase<=Length[centerBaseCandidates], iBase++,
    Print[{iBase, Length[centerBaseCandidates]}];

    newVectors = centerBaseCandidates[[iBase]];

    newLattice = getLattice[newVectors, limit];
    newOffset = If[useOffset, -1/2 * Total[newVectors], {0, 0, 0}];
    (* newLattice = Map[# + newOffset &, newLattice]; *)

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

    If[Length[ambiguousRegion] == 0,
        Print["Found"];
        Print[newVectors];
        Print[{iBase, Length[baseRegion], Length[ambiguousRegion]}];
    ];
];


(* Print[baseRegion];
Print[ambiguousRegion];
 *)
