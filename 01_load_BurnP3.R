

  BP3_gdb <- list.files(file.path(spatialDir, "BurnP3"), full.names = TRUE)[3]
  gdb_info <- rast(BP3_gdb)
  print(gdb_info)
  BP3P <- rast(BP3_gdb, subds = "BC_BurnP3_public_20years")
  
  plot(BP3P)
writeRaster(BP3P, file.path(spatialOutDir,'PB3P.tif'), overwrite=TRUE)