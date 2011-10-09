create.grid <- function(lon.range, lat.range, lon.n, lat.n, lon.obs, lat.obs)
{
    # finds the northernmost and southmost points within bands of width 2*d.lon 
    d.lon=1*2
    # finds the westmost and eastmost points within bands of width 2*d.lat
    d.lat=0.2*2
    lonnie=seq(lon.range[1], lon.range[2], length=lon.n)
    lattie=seq(lat.range[1], lat.range[2], length=lat.n)
    grid=expand.grid(lon=lonnie,lat=lattie)
    ok.lon=rep(F,dim(grid)[1])
    ok.lat=rep(F,dim(grid)[1])
    for (i in unique(grid$lon)){
        lim=range(lat.obs[lon.obs>=i-d.lon & lon.obs<=i+d.lon])
        ok.lat[grid$lon==i & grid$lat>=lim[1] & grid$lat<=lim[2]]=TRUE
    }
    for (i in unique(grid$lat)){
        lim=range(lon.obs[lat.obs>=i-d.lat & lat.obs<=i+d.lat])
        ok.lon[grid$lat==i & grid$lon>=lim[1] & grid$lon<=lim[2]]=TRUE
    }
    grid=data.frame(grid,ok.lon,ok.lat)
    names(grid)[1:2]=c("shootlong","shootlat")
    list(grid=grid, lonnie=lonnie, lattie=lattie)
}
