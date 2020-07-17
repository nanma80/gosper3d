#!/usr/bin/env wolframscript

normalizedInnerProd[u_, v_] := u.v/Norm[u]/Norm[v];
getLattice[vectors_, limit_] := Flatten[
    Table[
        {i, j, k}.vectors, {i, -limit, limit}, {j, -limit, limit}, {k, -limit, limit}
    ], 2];

get4dCubicLattice[limit_] := Flatten[Table[
    {i, j, k, l}, {i, -limit, limit}, {j, -limit, limit}, {k, -limit, limit}, {l, -limit, limit}
], 3];

get4dZeroSum[limit_] := Select[get4dCubicLattice[limit], Total[#]==0&];

allPoints3d = {};
projectionMatrix = Orthogonalize[{{1, 1, 1, 1}, {1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}}][[{2, 3, 4}]];

For[limit = 1, limit <= 4, limit++,
    points = get4dZeroSum[limit];
    points3d = N[points.Transpose[projectionMatrix]];
    AppendTo[allPoints3d, points3d];
];

dataFolder = "data";
outputFileName = FileNameJoin[{dataFolder, "json", "fcc_from_4d.json"}];

If[FileExistsQ[outputFileName], DeleteFile[outputFileName] ];
Export[outputFileName, allPoints3d, "RawJSON", Compact -> True];
Print["saved json data to " <> outputFileName];
