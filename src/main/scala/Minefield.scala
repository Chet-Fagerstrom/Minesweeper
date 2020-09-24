package scala

class Minefield(m: Int, h: Int, w: Int, val fld: List[Tile]) {

    def getFlippedTiles(): List[Tile] = {
        fld.filter(_.flipped == true)
    }

    def isMineAt(x: Int, y: Int): Boolean = {
        if(x <= 0 || y <= 0 || x > w || y > h) false
        else{
            val thisTile = getTileAt(x,y)
            thisTile.hasMine
        }
    }

    def getTileAt(x: Int, y: Int): Tile = {
        fld.find(t => t.x == x && t.y == y).get
    }

    def isCleared(): Boolean = {
        getFlippedTiles().length == fld.length - m
    }

    def getSurr(x: Int, y: Int): Int = {
        val thisTile = getTileAt(x,y)
        (for(x <- x-1 to x+1; y <- y-1 to y+1) yield {//checks 8 surrounding tiles
            if(isMineAt(x,y)) 1
            else 0
        }).sum
    }
}

object Minefield{
    def apply(m: Int, h: Int, w: Int): Minefield = {
        val locs = (for(x <- List.range(1, w + 1); y <- List.range(1, h + 1)) yield (x,y))
        val mineLocs = util.Random.shuffle(locs).take(m)
        val tiles = locs.map {case (x,y) => new Tile(x,y,mineLocs.contains((x,y)))}
        //println(tiles.map(_.toString()).mkString("\n"))
        new Minefield(m, h, w, tiles)
    }
}
