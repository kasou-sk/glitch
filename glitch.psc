record PathVertex {
    ts: Int
    lon: Double
    lat: Double
}

def toRadians(d) = d/180*pi
def toDegrees(r) = r/pi*180

def angularDistance(vx1:PathVertex, vx2:PathVertex): Double {
    var dlon = lon2 - lon1
    var dlat = lat2 - lat1
    var lat1 = toRadians(vx1.lat)
    var lat2 = toRadians(vx2.lat)
    var lon1 = toRadians(vx1.lon)
    var lon2 = toRadians(vx2.lon)
    def square(x) = x * x
    def hav(f) = square(sin(f / 2))
    return 2 * asin(sqrt(hav(dlat) + cos(lat1) * cos(lat2) * hav(dlon)))
}

def sphericalAngleCos(a:Double, b:Double, c:Double): Double {
    var cosA = (cos(c) - cos(a) * cos(b)) / (sin(a) * sin(b))
    return max(-1, min(1, cosA))
}

def distance(vx1:PathVertex, vx2:PathVertex): Double {
    var r = 6371000.0
    return r * angularDistance(vx1, vx2)
}

record InnerItem {
    prev :: PathVertex
    self :: PathVertex
    next :: PathVertex 
}

def middlePoint(vx1:PathVertex, vx3:PathVertex, ts2:Int) : PathVertex {
    var dts13 = vx3.ts - vx1.ts
    var dts12 = ts2 - vx1.ts
    var dts23 = vx3.ts - ts2
    var middleLon = (dts23 * vx1.lon + dts12 * vx3.lon) / dts13
    var middleLat = (dts23 * vx1.lat + dts12 * vx3.lat) / dts13
    return PathVertex {
        ts = ts2
        lon = middleLon
        lat = middleLat
    }
}

def deviation(iv:InnterItem): Double {
    var vxM = middlePoint(iv.prev, iv.next, iv.self.ts)
    return distance(iv.self, vxM)
}

def angleCos(iv:InnerItem): Double {
    var a = angularDistance(iv.prev, iv.self)
    var b = angularDistance(iv.self, iv.next)
    var c = angularDistance(iv.prev, iv.next)
    return sphericalAngleCos(a, b, c)
}

def speed(iv: InnerItem): Double {
    return (distance(iv.prev, iv.self) + distance(iv.self, iv.next)) / (iv.next.ts - iv.prev.ts)

def isGlitch(iv:InnerItem): Bool {
    return glitchFactor(iv) > 0.1
}

def glitchFactor(iv: InnerItem): Double {
    var d = deviation iv
    var a = angleCos iv
    var s = speed iv
    return d*(1+a)*(1+a)/sqrt(s)
}

Step 1: Cut stream of GPS fixes into new segment when one of the following conditions are met:
        a) previous fix if more than 60 seconds older, i.e. timestamp of current fix is at least 61 seconds newer than previously recorded fix.
        b) both previous and current fixes share the very same longitude and latitude, i.e. truck is standing still and it will messup equations

Step 2: Throw away segments shorter than 3 vertices

Step 3: Start accumulate vertices until there are at least 6 of them.

Step 4: When we have 6 vertices in buffer, we may send first two to DB, but still keep them in buffer (possibly mark as already sent?)
        Let's call 2nd to 5th vertices as inner ones, i.e. they have both previous and next vertex in buffer.

Step 5: If both 3rd vertex and 4th vertex are possible glitches (their glitch factor > 0.1), choose the one with lower average glitch factor of remaining 3 inner vertices.
        If currect average glitch factor of all 4 inner vertices is higher than without choosen one, drop him and accumulate next vertex / repeat from step 3.

Step 6: If only 3rd vertex is glitch, check whether average glitch factor of 2nd, 3rd and 4th vertex is higher than average of 2nd and 4th vertex.
        If so, drop 3rd one and repeat from step 3.

Step 7: If none of vertices is glitch, drop 1st vertex (it's already sent do DB) and repeat from step 3.

Step 8: If there are no more vertices in this segment, finish sending of still not sent vertices to DB and repeat from step 2.

