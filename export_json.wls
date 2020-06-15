#!/usr/bin/env wolframscript

getPolyhedronData[polyhedron_] := <|
    "vertices" -> polyhedron[[1]],
    "faceIndices" -> polyhedron[[2]] - 1
|>;

shape = "bcc_27a_regions";
dataFolder = "data";
dataFileName = FileNameJoin[{dataFolder, "wl", shape <> ".wl"}];
outputFileName = FileNameJoin[{dataFolder, "json", shape <> ".json"}];

inputData = Get[dataFileName];

regionsByGeneration = inputData["regionsByGeneration"];
centersByGeneration = inputData["centersByGeneration"];

sa=SurfaceArea/@regionsByGeneration;

Print[sa[[2]]/sa[[1]]];


jsonData = 
    <|
    "regionsByGeneration" -> Map[getPolyhedronData, regionsByGeneration],
    "centersByGeneration" -> centersByGeneration
    |>;

If[FileExistsQ[outputFileName], DeleteFile[outputFileName] ];
Export[outputFileName, jsonData, "RawJSON", Compact -> True];
Print["saved json data to " <> outputFileName];
