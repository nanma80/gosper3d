#!/usr/bin/env wolframscript

exportToPov = True;

Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

thickness = 0.1;
lighting = {{"Point", White, {10, 10, 10}}};
outputFolder = "output";

normalizedInnerProd[u_, v_] := u.v/Norm[u]/Norm[v];
getLattice[vectors_, limit_] := Flatten[
    Table[
        {i, j, k}.vectors, {i, -limit, limit}, {j, -limit, limit}, {k, -limit, limit}
    ], 2];

get3dCubicLattice[limit_] := Flatten[Table[
    {i, j, k}, {i, -limit, limit}, {j, -limit, limit}, {k, -limit, limit}
], 2];

get3dZeroSum[limit_] := Select[get3dCubicLattice[limit], Total[#]==0&];

limit = 3;

points = get3dZeroSum[limit];

range = limit * 1.2;

graph = Graphics3D[
    {
        {
            FaceForm[White], Map[Sphere[#, thickness * 3]&, points]
        }
    },
    ViewPoint -> {1, 1, 1} * limit * 10,
    PlotRange -> {{-range, range}, {-range, range}, {-range, range}},
    Lighting -> lighting
];


imageSize = {400, 300};

outputFileName = "hex_from_3d_" <> IntegerString[limit] <> ".png";
If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];
outputFileName = FileNameJoin[{outputFolder, outputFileName}];

cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

If[exportToPov,
    If[FileExistsQ[outputFileName], DeleteFile[outputFileName]];
    POVRayRender[graph, Method -> "Triangle", OutputPath -> outputFileName, ImageSize -> imageSize, RenderOptions -> "+A0.001 -J"];
    Run[cropCommand];
    ,
    Export[outputFileName, graph];
];
Print["Saved the image to " <> outputFileName];
