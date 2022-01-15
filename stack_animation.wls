#!/usr/bin/env wolframscript

exportToPov = True;

imageSize = {4, 3} * 480 / 3;
movingFrameCount = 4;
(* movingFrameCount = 51; *)

argv = Rest @ $ScriptCommandLine;
nGenerations = If[Length[argv] >= 1, ToExpression[argv[[1]]], 2];

shape = "srd";
shapeWithParams = shape <> "_" <> IntegerString[nGenerations];

dataFolder = "data";
imageFolder = "output";
transparentThreshold = 3.5;
Needs["POVRayRender`"];
ConfigurePOVRayRender["POVRayPath" -> "C:\\Program Files\\POV-Ray\\v3.7\\bin\\pvengine64.exe"];

HInner[v_, u_] := 2 * v[[1]] * u[[1]] - Dot[v, u];
HNormSquare[v_] := HInner[v, v];
HNorm[v_] := Sqrt[HNormSquare[v]];
Rotation[t_] :=
{
  {1,  0, 0},
  {0, Cos[t], Sin[t]},
  {0, -Sin[t], Cos[t]}
};
epsilon = 0.00000001;
HReflect[point_, mirror_] := point - 2 * HInner[point, mirror]/HInner[mirror, mirror]*mirror;
HDoubleReflect[point_, mirror1_, mirror2_] := HReflect[HReflect[point, mirror1], mirror2];
(* ApproxSamePoint[point1_, point2_] := Norm[N[point1 - point2]] < 0.00001; *)
ApproxSamePoint[point1_, point2_] := Round[point1, epsilon] == Round[point2, epsilon];

getEdgesFromFace[face_] := Table[{face[[i+1]], face[[Mod[i+1, Length[face]] + 1]]}, {i, 0, Length[face] - 1}];
sameCenter[set1_, set2_] := ApproxSamePoint[Total[set1], Total[set2]];
(* sameEdge[e1_, e2_] := sameCenter[e1, e2] && ((ApproxSamePoint[e1[[1]], e2[[1]]] && ApproxSamePoint[e1[[2]], e2[[2]]] ) || (ApproxSamePoint[e1[[1]], e2[[2]]] && ApproxSamePoint[e1[[2]], e2[[1]]] )); *)
sameEdge[e1_, e2_] := sameCenter[e1, e2];

getKlein[v_] := {0, v[[2]], v[[3]]}/v[[1]];
getPoincare[v_] := {0, v[[2]], v[[3]]}/(1+v[[1]]);
splitEdge[edge_, n_] := Table[{k edge[[1]] + (1-k) edge[[2]], (k+1/n) edge[[1]] + (1-k-1/n) edge[[2]] }, {k, 0, 1 - 1/n, 1/n}];
HNormalize[v_, norm_] := v/HNorm[v] * norm;
getHyperboloid[v_] := {-v[[1]], v[[2]], v[[3]]};

scale[edge_, ratio_] := Module[
    {center},
    center = Mean[edge];
    Map[(# - center) * ratio + center&, edge]
];

scaleFace[face_, ratio_, stellationLevel_] := Module[
    {edges, edgeOrders},
    edges = Map[scale[#, ratio]&, getEdgesFromFace[face]];
    edgeOrders = Table[Mod[stellationLevel * k, p]+1, {k, 0, p-1}];
    (* edges[[edgeOrders]] *)
    (* Map[#[[1]]&, edges[[edgeOrders]]] *)
    Flatten[edges[[edgeOrders]], 1]
];



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

viewMatrix = RotationMatrix[Pi/180 * 33, {0, 0, 1}];

exportFrame[region_, centers_, explosionFactor_, scaleFactor_, viewRangeFactor_, saturationFactor_, frameIndex_, outputFolderName_] := Module[
    {},

    srds = Map[
        translatedRegion[region, explosionFactor * #]&,
        centers
    ];

    elements = {};
    centerMax = Max[Max[centers]];
    centerMaxNorm = Max[Map[Norm, centers]];

    centerVertices = {};
    For[centerIndex = 1, centerIndex <= Length[centers], centerIndex++,
        polyhedron = srds[[centerIndex]];
        center = centers[[centerIndex]];
        vertices = scaleFactor * (polyhedron[[1]].viewMatrix);
        If[Norm[center] == 0, centerVertices = vertices];
        faceIndices = polyhedron[[2]];
        faces = Map[vertices[[#]]&, faceIndices];
        edges = Flatten[Map[getEdgesFromFace, faces], 1];
        saturation = Min[ Norm[center]/centerMaxNorm, saturationFactor];
        color = Hue[centerIndex/Length[centers], saturation, 1];
        opacity = If[Norm[center] == 0, 1,
            If[explosionFactor > transparentThreshold, 0,
                If[explosionFactor > transparentThreshold-1, transparentThreshold - explosionFactor,
                    1
                ]
            ]
        ];

        AppendTo[elements, {Opacity[opacity], FaceForm[Lighter[color, 0.1]], Map[Polygon, faces]}];
    ];

    range = Max[Norm/@centerVertices] * 3 * 0.75 / scaleFactor * viewRangeFactor;
    lighting = {{"Point", White, {1, -1, 1} * range}};
    thickness = range * 0.003;

    graph = Graphics3D[
            elements,
            ViewPoint -> {3, -3, 1.6} * 100,
            PlotRange -> {{-range, range}, {-range, range}, {-range, range}},
            Lighting -> lighting
        ];


    outputFolder = FileNameJoin[{imageFolder, outputFolderName}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

    outputFileName = "stacking_frame_" <> IntegerString[frameIndex, 10, 4] <> ".png";
    outputFileName = FileNameJoin[{outputFolder, outputFileName}];

    cropCommand = "mogrify -crop " <> IntegerString[imageSize[[2]]] <> "x" <> IntegerString[imageSize[[2]]] <> "+" <> IntegerString[(imageSize[[1]]-imageSize[[2]])/2] <> "+0 " <> outputFileName;

    If[exportToPov,
      If[FileExistsQ[outputFileName], DeleteFile[outputFileName]];
      POVRayRender[graph, Method -> "Triangle", OutputPath -> outputFileName, ImageSize -> imageSize, RenderOptions -> "+A0.001 -J +UA"];
      Run[cropCommand];
      ,
      Export[outputFileName, graph];
    ];
    Print["Saved an image to " <> outputFileName];
];

exportAnimation[] := Module[
    {
        outputFolder,
        gifFileName,
        gifFolder,
        maxExplosionFactor,
        explodeStep,
        explosionFactors,
        frameIndex,
        explosionFactor,
        gifCommand
    },

    outputFolderName = shapeWithParams <> "_stacking_frames";
    outputFolder = FileNameJoin[{imageFolder, outputFolderName}];
    If[!DirectoryQ[outputFolder], CreateDirectory[outputFolder]];

    gifFileName = shapeWithParams <> "_stacking";
    gifFolder = FileNameJoin[{imageFolder, shape}];
    If[!DirectoryQ[gifFolder], CreateDirectory[gifFolder]];

    explosionFactors = Table[(1 - Sin[k/(movingFrameCount-1) Pi/2])*3 + 1 , {k, 0, movingFrameCount-1, 1}]//N;

    stayingFrameCount = Floor[movingFrameCount/6];
    shrinkingFrameCount = Floor[movingFrameCount/2];
    totalFrameCount = nGenerations*(Length[explosionFactors] + stayingFrameCount + shrinkingFrameCount) - shrinkingFrameCount;

    frameCount = 1;
    For[genIndex = 1, genIndex <= nGenerations, genIndex++,
        regions = regionsByGeneration[[genIndex]];
        centers = centersByGeneration[[genIndex]];
        scaleFactor = (1/3)^(genIndex - 1);

        For[exIndex = 1, exIndex <= Length[explosionFactors], exIndex++,
            explosionFactor = explosionFactors[[exIndex]];
            Print["Frame #" <> IntegerString[frameCount] <> "/" <> IntegerString[totalFrameCount] <> " Gen " <> ToString[genIndex] <> " explosionFactor " <> ToString[explosionFactor] <> " " <> outputFolderName];

            exportFrame[regions, centers, explosionFactor, scaleFactor, scaleFactor, 1.0, frameCount, outputFolderName]; 
            frameCount ++;
        ];

        For[stayingIndex = 1, stayingIndex <= stayingFrameCount, stayingIndex++,
            Print["Frame #" <> IntegerString[frameCount] <> "/" <> IntegerString[totalFrameCount] <> " Gen " <> ToString[genIndex] <> " staying " <> ToString[stayingIndex] <> "/" <> ToString[stayingFrameCount] <> " " <> outputFolderName];

            exportFrame[regions, centers, 1, scaleFactor, scaleFactor, 1.0, frameCount, outputFolderName];
            frameCount ++;
        ];

        If[genIndex != nGenerations,
            For[shrinkingIndex = 1, shrinkingIndex <= shrinkingFrameCount, shrinkingIndex++,
                Print["Frame #" <> IntegerString[frameCount] <> "/" <> IntegerString[totalFrameCount] <> " Gen " <> ToString[genIndex] <> " shrinking " <> ToString[shrinkingIndex] <> "/" <> ToString[shrinkingFrameCount] <> " " <> outputFolderName];
                exportFrame[regions, centers, 1, scaleFactor*(1/3)^(shrinkingIndex/shrinkingFrameCount), scaleFactor, (shrinkingFrameCount-shrinkingIndex)/shrinkingFrameCount, frameCount, outputFolderName]
                frameCount ++;
            ];
        ];
    ];
    
    outputFileName = FileNameJoin[{imageFolder, outputFolderName, "stacking_frame_" <> IntegerString[totalFrameCount, 10, 4] <> ".png"}];
    
    For[finalIndex = 1, finalIndex <= movingFrameCount, finalIndex++,
        newFileName = FileNameJoin[{imageFolder, outputFolderName, "stacking_frame_" <> IntegerString[frameCount, 10, 4] <> ".png"}];
        copyCommand = "copy .\\" <> outputFileName <> " .\\" <> newFileName;
        Run[copyCommand];
        frameCount ++;
    ];

    delay = If[totalFrameCount >= 50, 3, Floor[500/totalFrameCount]];

    gifCommand = "magick convert -quiet +repage -alpha remove -loop 0 -delay " <> IntegerString[delay] <> " -dispose 2 " <> outputFolder <> "\\stacking_frame*.png " <> gifFolder <> "\\" <> gifFileName <> ".gif";

    Print[gifCommand];
    Run[gifCommand];
    Print["Saved the animation to " <> gifFolder <> "\\" <> gifFileName <> ".gif"];
    
    mp4Command = "ffmpeg -i " <> gifFolder <> "\\" <> gifFileName <> ".gif" 
      <> " -movflags faststart -pix_fmt yuv420p -vf \"scale=trunc(iw/2)*2:trunc(ih/2)*2\" "
      <> gifFolder <> "\\" <> gifFileName <> ".mp4";

    mp4FileName = gifFolder <> "\\" <> gifFileName <> ".mp4";
    If[FileExistsQ[mp4FileName], DeleteFile[mp4FileName]];
    Print[mp4Command];
    Run[mp4Command];
    Print["Saved the video to " <> gifFolder <> "\\" <> gifFileName <> ".mp4"];
];

dataFileName = FileNameJoin[{dataFolder, "wl", "bcc_27a_regions.wl"}];
data = Read[dataFileName];
regionsByGeneration = data["regionsByGeneration"];
centersByGeneration = data["centersByGeneration"];
(* add a 4th level of centers. There are a 4th level of regionsByGeneration *)
AppendTo[centersByGeneration, centersByGeneration[[Length[centersByGeneration]]] * 3];

exportAnimation[];

(* frameCount = 1;
For[genIndex = 1, genIndex <= 3, genIndex++,
    regions = regionsByGeneration[[genIndex]];
    centers = centersByGeneration[[genIndex]];
    scaleFactor = (1/3)^(genIndex - 1);

    exportFrame[regions, centers, 3.5, scaleFactor, scaleFactor, 1.0, frameCount, "test"]; frameCount ++;
    exportFrame[regions, centers, 3.0, scaleFactor, scaleFactor, 1.0, frameCount, "test"]; frameCount ++;
    exportFrame[regions, centers, 2.5, scaleFactor, scaleFactor, 1.0, frameCount, "test"]; frameCount ++;
    exportFrame[regions, centers, 2.0, scaleFactor, scaleFactor, 1.0, frameCount, "test"]; frameCount ++;
    exportFrame[regions, centers, 1.5, scaleFactor, scaleFactor, 1.0, frameCount, "test"]; frameCount ++;
    exportFrame[regions, centers, 1.0, scaleFactor, scaleFactor, 1.0, frameCount, "test"]; frameCount ++;
    exportFrame[regions, centers, 1.0, scaleFactor*(1/3)^(1/3.), scaleFactor, 0.66, frameCount, "test"]; frameCount ++;
    exportFrame[regions, centers, 1.0, scaleFactor*(1/3)^(2/3.), scaleFactor, 0.33, frameCount, "test"]; frameCount ++;
    exportFrame[regions, centers, 1.0, scaleFactor*1/3, scaleFactor, 0.0, frameCount, "test"]; frameCount ++;
]
*)
