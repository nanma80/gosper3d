#!/usr/bin/env wolframscript

skipBaseRegionGeneration = True;
altMode = False;
generationLimit = 3;
saveData = False;
plotImages = True;

dataFolder = "data";
imageFolder = "output";
imageSize = 150 * {4, 3};
exportToPov = True;
epsilon = 0.00000001;

Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

colors = {Blue, Red, Yellow, Green};

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
scaledRegion[region_, scale_] := TransformedRegion[region, (# * scale)&];

plotRegion[region_, fileName_, color_] := Module[
    {},

    polyhedron = region;
    outputFileName = fileName;

    vertices = polyhedron[[1]];
    faceIndices = polyhedron[[2]];
    faces = Map[vertices[[#]]&, faceIndices];
    edges = Flatten[Map[getEdgesFromFace, faces], 1];

    range = Max[Norm/@vertices] * 0.75;
    lighting = {{"Point", White, {1, -1, 1} * range}};
    thickness = range * 0.003;

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
    Print["saved image to " <> outputFileName];
];

origin = {0, 0, 0};

baseRegion = {{-2, 0, -2}, {0, -2, -2}, {0, 0, -2}, {0, 2, -2}, {2, 0, -2}, {-1, -1, -1}, {-1, 1, -1}, {1, -1, -1}, {1, 1, -1}, {-2, -2, 0}, {-2, 0, 0}, {-2, 2, 0}, {0, -2, 0}, {0, 0, 0}, {0, 2, 0}, {2, -2, 0}, {2, 0, 0}, {2, 2, 0}, {-1, -1, 1}, {-1, 1, 1}, {1, -1, 1}, {1, 1, 1}, {-2, 0, 2}, {0, -2, 2}, {0, 0, 2}, {0, 2, 2}, {2, 0, 2}};

scalingMatrixA = IdentityMatrix[3] * 3;
scalingMatrixB = RotationMatrix[Pi/3, {1, 1, 1}] * 3;

srd = getsrd[3];
regionsByGeneration = {srd};
centers = baseRegion;
centersByGeneration = {};

If[plotImages,
        plotRegion[regionsByGeneration[[1]],
        "srd" <> IntegerString[0] <> ".png", 
        colors[[1]]];
    ];

For[generationIndex = 1, generationIndex <= generationLimit, generationIndex++,
    Print["generationIndex"];
    Print[generationIndex];
    centers = centers.If[altMode, scalingMatrixB, scalingMatrixA];
    AppendTo[centersByGeneration, centers];
    currentRegion = regionsByGeneration[[generationIndex]];

    srds = Map[
        translatedRegion[currentRegion, #]&,
        centers
    ];

    Print["Unioning this number of regions:"];
    Print[Length[srds]];

(*     For[i = 1, i <= Length[srds], i++,
        For[j = 1, j <= i - 1, j++,
            intVolume = Volume[RegionIntersection[srds[[i]], srds[[j]] ]]//N;
            If[intVolume > epsilon,
                Print[ToString[i] <> " " <> ToString[j] <> " " <> ToString[intVolume]]
            ];
        ];
    ];

    Print["Checked intersection"];
 *)
    unionSrds = srds[[1]];
    For[i = 1, i <= Length[srds], i++,
        (* Print[ToString[i] <> " " <> ToString[Volume[RegionIntersection[currentRegion, srds[[i]]]]]]; *)
        unionSrds = RegionUnion[unionSrds, srds[[i]]];
        (* unionSrds//Volume *)
    ];

    Print[{ Volume[currentRegion] * Length[srds], Volume[unionSrds], Volume[currentRegion] * Length[srds] - Volume[unionSrds]} ];
    AppendTo[regionsByGeneration, unionSrds];
    testSize = 13/9 * 3^(generationIndex);
    testVolume = testSize^3;
    intVolume = Volume[RegionIntersection[Cuboid[{0,0,0}, {testSize, testSize, testSize}], regionsByGeneration[[generationIndex + 1]]]];
    Print[{
        testVolume//N,
        intVolume//N,
        testVolume - intVolume//N
    }];

    If[plotImages,
        scaled = scaledRegion[regionsByGeneration[[generationIndex + 1]], 1/3^(generationIndex)];
        plotRegion[scaled,
            "srd" <> IntegerString[generationIndex] <> ".png", 
        colors[[generationIndex + 1]]];
    ];
];

If[saveData,
    dataFileName = FileNameJoin[{dataFolder, "wl", "bcc_27a_regions.wl"}];
    outputData = <|
        "regionsByGeneration" -> regionsByGeneration,
        "centersByGeneration" -> centersByGeneration
    |>;

    If[FileExistsQ[dataFileName], DeleteFile[dataFileName]];
    Save[dataFileName, outputData];
    Print["Saved cells to " <> dataFileName];
];


(* plotRegion[regionsByGeneration[[1]], "srd0.png", Blue]; *)