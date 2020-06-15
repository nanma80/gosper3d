#!/usr/bin/env wolframscript

skipBaseRegionGeneration = True;
altMode = False; (* altMode not working well. The regions intersect *)

imageFolder = "output";
imageSize = 150 * {4, 3};
exportToPov = True;

Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];


normalizedInnerProd[u_, v_] := u.v/Norm[u]/Norm[v];
getLattice[vectors_, limit_] := Flatten[
    Table[
        {i, j, k}.vectors, {i, -limit, limit}, {j, -limit, limit}, {k, -limit, limit}
    ], 2];
offset[vectors_, offset_] := Map[ (# + offset) &, vectors];
getEdgesFromFace[face_] := Table[{face[[i+1]], face[[Mod[i+1, Length[face]] + 1]]}, {i, 0, Length[face] - 1}];
sameCenter[edge1_, edge2_] := Total[edge1] == Total[edge2];
getsrd[r_] := Module[
    {s, pyramidpx, pyramidnx, pyramidpy, pyramidny, pyramidpz, pyramidnz},
    s = Norm[{0, -r, r}- {r, 0, 0}];
    pyramidpx = Pyramid[{{0, r, r}, {0, r, -r}, {0, -r, -r}, {0, -r, r}, {r, 0, 0}}];
    pyramidnx = Pyramid[{{0, r, r}, {0, r, -r}, {0, -r, -r}, {0, -r, r}, {-r, 0, 0}}];

    pyramidpy = Pyramid[{{r, 0, r}, {r, 0, -r}, {-r, 0, -r}, {-r, 0, r}, {0, r, 0}}];
    pyramidny = Pyramid[{{r, 0, r}, {r, 0, -r}, {-r, 0, -r}, {-r, 0, r}, {0, -r, 0}}];

    pyramidpz = Pyramid[{{r, r, 0}, {r, -r, 0}, {-r, -r, 0}, {-r, r, 0}, {0, 0, r}}];
    pyramidnz = Pyramid[{{r, r, 0}, {r, -r, 0}, {-r, -r, 0}, {-r, r, 0}, {0, 0, -r}}];

    srd = RegionUnion[pyramidpx, pyramidnx, pyramidpy, pyramidny, pyramidpz, pyramidnz];
    srd
];

translatedRegion[region_, offset_] := TransformedRegion[region, (# + offset)&];

plotRegion[region_, fileName_, color_] := Module[
    {polyhedron, outputFileName, vertices, faceIndices, faces, edges, range, lighting
        , thickness, graph, cropCommand},

    polyhedron = region;
    outputFileName = If[altMode, "alt_", ""] <> fileName;

    vertices = polyhedron[[1]];
    faceIndices = polyhedron[[2]];
    faces = Map[vertices[[#]]&, faceIndices];
    edges = Flatten[Map[getEdgesFromFace, faces], 1];

    range = Max[Norm/@vertices] * 0.75;
    lighting = {{"Point", White, {1, -1, 1} * range * 4}};
    thickness = range * 0.01;

    graph = Graphics3D[
            {
                {
                    Opacity[1], FaceForm[Lighter[color, 0.1]], Map[Polygon, faces]
                },

                {
                    Opacity[1], FaceForm[color], Map[Cylinder[#, thickness]&, edges]
                },
             
                {
                    Opacity[1], FaceForm[Darker[color]], Map[Sphere[#, thickness * 2]&, vertices]
                }
            },
            ViewPoint -> {3, -4, 2}*3,
            PlotRange -> {{-range, range}, {-range, range}, {-range, range}},
            Lighting -> lighting
        ];

    outputFileName = FileNameJoin[{imageFolder, outputFileName}];
    cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

    If[exportToPov,
        If[FileExistsQ[outputFileName], DeleteFile[outputFileName]];
        POVRayRender[graph, Method -> "Triangle", OutputPath -> outputFileName, ImageSize -> imageSize, RenderOptions -> "+A0.001 -J +UA"];
        Run[cropCommand];
        ,
        Export[outputFileName, graph];
    ];
];

origin = {0, 0, 0};
axis = {1, 1, 1};

baseVectors = {
    {1, 1, 1},
    {1, 1, -1},
    {1, -1, 1}
};

limit = 6;

baseLattice = getLattice[baseVectors, limit];

newVectors = If[altMode, 
    {{-5, 1, 1}, {1, -5, 1}, {1, 1, -5}},
    3 * baseVectors
];

newLattice = getLattice[newVectors, limit];

baseRegion = {{-2, 0, -2}, {0, -2, -2}, {0, 0, -2}, {0, 2, -2}, {2, 0, -2}, {-1, -1, -1}, {-1, 1, -1}, {1, -1, -1}, {1, 1, -1}, {-2, -2, 0}, {-2, 0, 0}, {-2, 2, 0}, {0, -2, 0}, {0, 0, 0}, {0, 2, 0}, {2, -2, 0}, {2, 0, 0}, {2, 2, 0}, {-1, -1, 1}, {-1, 1, 1}, {1, -1, 1}, {1, 1, 1}, {-2, 0, 2}, {0, -2, 2}, {0, 0, 2}, {0, 2, 2}, {2, 0, 2}};

scalingMatrixA = IdentityMatrix[3] * 3;
scalingMatrixB = RotationMatrix[Pi/3, {1, 1, 1}] * 3;
scalingMatrix = If[altMode, scalingMatrixB, scalingMatrixA];

srd = getsrd[3];

plotRegion[srd, "srd0.png", Blue];

regionsInGenerations = {srd};

centersA = baseRegion.scalingMatrix;
srds = Map[
    translatedRegion[srd, #]&,
    centersA
];

unionSrds = srds[[1]];
For[i = 1, i <= Length[srds], i++,
    Print[ToString[i] <> " " <> ToString[Volume[RegionIntersection[srd, srds[[i]]]]]];
    unionSrds = RegionUnion[unionSrds, srds[[i]]];
];

cubeRegion = Cube[6];
Volume[unionSrds]/Volume[srd]
Volume[RegionDifference[cubeRegion, unionSrds]]

plotRegion[unionSrds, "srd1.png", Red];

(* Gen 2 *)
centersA2 = baseRegion.scalingMatrix.scalingMatrix;
srds2 = Map[
    translatedRegion[unionSrds, #]&,
    centersA2
];

unionSrds2 = srds2[[1]];
For[i = 1, i <= Length[srds2], i++,
    Print[ToString[i] <> " " <> ToString[Volume[RegionIntersection[unionSrds, srds2[[i]]]]]];
    unionSrds2 = RegionUnion[unionSrds2, srds2[[i]]];
];

Volume[unionSrds2]/Volume[srd]
Volume[unionSrds2]/Volume[unionSrds]
Volume[RegionDifference[Cube[12], unionSrds2]]

plotRegion[unionSrds2, "srd2.png", Yellow];

(* Gen 3 *)
centersA3 = baseRegion.scalingMatrix.scalingMatrix.scalingMatrix;
srds3 = Map[
    translatedRegion[unionSrds2, #]&,
    centersA3
];

unionSrds3 = srds3[[1]];
For[i = 1, i <= Length[srds3], i++,
    Print[ToString[i] <> " " <> ToString[Volume[RegionIntersection[unionSrds2, srds3[[i]]]]]];
    unionSrds3 = RegionUnion[unionSrds3, srds3[[i]]];
];
(* 
Volume[unionSrds3]/Volume[srd]
Volume[unionSrds3]/Volume[unionSrds]
Volume[unionSrds3]/Volume[unionSrds2]

Volume[RegionDifference[Cube[18], unionSrds3]]
 *)
plotRegion[unionSrds3, "srd3.png", Green];


(* graph = Graphics3D[srd]; *)
(* graph = Graphics3D[unionSrds]; *)
(* graph = Graphics3D[unionSrds2]; *)
(* graph = Graphics3D[unionSrds3]; *)
