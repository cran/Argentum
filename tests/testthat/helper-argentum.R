# Shared fixtures. Nothing here touches the network.

local_quiet <- function(env = parent.frame()) {
  withr::local_options(argentum.quiet = TRUE, .local_envir = env)
}

# Point the cache somewhere disposable so tests never read or write the
# user's real cache directory.
local_temp_cache <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(R_USER_CACHE_DIR = dir, .local_envir = env)
  argentum_cache_clear("memory")
  withr::defer(argentum_cache_clear("memory"), envir = env)
  dir
}

# A slice of IDERA's sheet: long format, one row per (organization, service).
idera_csv <- function() {
  paste0(
    "NIVEL JURISDICCIONAL,JURISDICCION,DEPENDENCIA,INSTITUCION/ ORGANISMO,",
    "TIPO DE GEOSERVICIO,URL,FECHA DE MODIFICACION\n",
    "Local,Buenos Aires,Buenos Aires,Municipalidad de Balcarce,WFS,",
    "https://geo.ideba.gob.ar/balcarce/wfs,2025-01-02\n",
    "Local,Buenos Aires,Buenos Aires,Municipalidad de Balcarce,WMS,",
    "https://geo.ideba.gob.ar/balcarce/wms,\n",
    "Local,Buenos Aires,Buenos Aires,Municipalidad de Balcarce,CSW,",
    "https://geonetwork.ideba.gob.ar/srv/eng/csw,\n",
    "Local,Corrientes,Corrientes,Municipalidad de Corrientes,WMS,",
    "https://gis.ciudaddecorrientes.gov.ar:8282/geoserver/wms,\n",
    "Nacional,,\"Ministerio de Economia, Ministerio de Defensa\",IGN,WMS,",
    "https://wms.ign.gob.ar/geoserver/wms,\n",
    "Universidad,,,UNCOMA,WFS,https://opendata.fi.uncoma.edu.ar/geoserver/wfs,\n"
  )
}

# The same sheet after a few months of hand editing: inconsistent capitals,
# a service type written as prose, a protocol pair in one cell, and a stale
# row repeated below the good one.
idera_messy_csv <- function() {
  paste0(
    "NIVEL JURISDICCIONAL,JURISDICCION,DEPENDENCIA,INSTITUCION/ ORGANISMO,",
    "TIPO DE GEOSERVICIO,URL,FECHA DE MODIFICACION\n",
    "Nacional,,IGN,IGN,WMS,https://wms.ign.gob.ar/geoserver/wms,\n",
    "nacional,,IGN,IGN,WFS,https://wms.ign.gob.ar/geoserver/wfs,2025-03-01\n",
    "NACIONAL ,,IGN,IGN,Servicio WCS,https://wms.ign.gob.ar/geoserver/wcs,\n",
    "Nacional,,INTA,INTA,WMS/WFS,https://geo.inta.gob.ar/geoserver/ows,\n",
    "Nacional,,INTA,INTA,WMS,https://vieja.inta.gob.ar/wms,\n"
  )
}

fake_catalog <- function() {
  data.frame(
    level        = c("Nacional", "Provincial", "Local"),
    jurisdiction = c("", "Cordoba", "Buenos Aires"),
    dependency   = c("", "", ""),
    organization = c("IGN", "Catastro X", "Solo Catalogo"),
    updated      = c("2025-03-01", "", ""),
    wms_url      = c("https://example.org/wms", "https://catastro.example/wms", ""),
    wfs_url      = c("https://example.org/wfs", "https://catastro.example/wfs", ""),
    wcs_url      = c("", "", ""),
    csw_url      = c("", "", "https://catalogo.example/csw"),
    stringsAsFactors = FALSE
  )
}

# The shape argentum <= 2.0.0 wrote, which users still have on disk.
legacy_catalog <- function() {
  data.frame(
    category     = c("Nacional", "Provincial"),
    organization = c("IGN", "Catastro X"),
    wms_url      = c("https://example.org/wms", "https://catastro.example/wms"),
    wfs_url      = c("https://example.org/wfs", ""),
    stringsAsFactors = FALSE
  )
}

with_fake_catalog <- function(code, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  utils::write.csv(fake_catalog(), path, row.names = FALSE, na = "")
  withr::local_options(argentum.catalog = path, .local_envir = env)
  force(code)
}

wfs_capabilities_xml <- function() {
  xml2::read_xml(
    '<WFS_Capabilities version="2.0.0">
       <FeatureTypeList>
         <FeatureType>
           <Name>ign:provincia</Name>
           <Title>Provincias</Title>
           <Abstract>Limites provinciales</Abstract>
           <DefaultCRS>urn:ogc:def:crs:EPSG::22185</DefaultCRS>
           <OtherCRS>urn:ogc:def:crs:EPSG::4326</OtherCRS>
           <WGS84BoundingBox>
             <LowerCorner>-73.6 -55.1</LowerCorner>
             <UpperCorner>-53.6 -21.8</UpperCorner>
           </WGS84BoundingBox>
         </FeatureType>
         <FeatureType>
           <Name>ign:localidad</Name>
           <DefaultCRS>urn:ogc:def:crs:EPSG::4326</DefaultCRS>
         </FeatureType>
       </FeatureTypeList>
     </WFS_Capabilities>'
  )
}

wms_capabilities_xml <- function() {
  xml2::read_xml(
    '<WMS_Capabilities version="1.3.0">
       <Capability>
         <Request>
           <GetMap>
             <Format>image/jpeg</Format>
             <Format>image/png</Format>
           </GetMap>
         </Request>
         <Layer>
           <Title>Container without a Name</Title>
           <CRS>EPSG:4326</CRS>
           <Layer>
             <Name>capabaseargenmap</Name>
             <Title>Mapa base</Title>
             <CRS>EPSG:3857</CRS>
             <EX_GeographicBoundingBox>
               <westBoundLongitude>-73.6</westBoundLongitude>
               <southBoundLatitude>-55.1</southBoundLatitude>
               <eastBoundLongitude>-53.6</eastBoundLongitude>
               <northBoundLatitude>-21.8</northBoundLatitude>
             </EX_GeographicBoundingBox>
           </Layer>
         </Layer>
       </Capability>
     </WMS_Capabilities>'
  )
}

as_capabilities <- function(xml, service, version, url = "https://example.org/ows") {
  structure(
    xml,
    class = c("argentum_capabilities", class(xml)),
    service = service, version = version, url = url
  )
}

# How GeoServer actually answers: a prefixed namespace on every element. This
# is the shape that made "//FeatureType" match nothing.
wfs_capabilities_prefixed <- function() {
  xml2::read_xml(
    '<wfs:WFS_Capabilities xmlns:wfs="http://www.opengis.net/wfs/2.0" version="2.0.0">
       <wfs:FeatureTypeList>
         <wfs:FeatureType>
           <wfs:Name>ide:rutas</wfs:Name>
           <wfs:Title>Rutas</wfs:Title>
           <wfs:DefaultCRS>urn:ogc:def:crs:EPSG::22185</wfs:DefaultCRS>
           <wfs:OtherCRS>urn:ogc:def:crs:EPSG::4326</wfs:OtherCRS>
         </wfs:FeatureType>
       </wfs:FeatureTypeList>
     </wfs:WFS_Capabilities>'
  )
}

# WMS 1.1.1: no namespace at all, root spelled differently, SRS instead of CRS,
# and several codes inside one element.
wms_capabilities_111 <- function() {
  xml2::read_xml(
    '<WMT_MS_Capabilities version="1.1.1">
       <Service><Title>IDE Vieja</Title></Service>
       <Capability>
         <Request><GetMap><Format>image/gif</Format></GetMap></Request>
         <Layer>
           <Title>root</Title>
           <SRS>EPSG:4326 EPSG:22185</SRS>
           <Layer><Name>vieja:1</Name><Title>Capa vieja</Title></Layer>
         </Layer>
       </Capability>
     </WMT_MS_Capabilities>'
  )
}
