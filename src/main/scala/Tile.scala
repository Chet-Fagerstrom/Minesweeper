package scala

class Tile(val x: Int, val y: Int, val hasMine: Boolean) {
    private var _flipped = false
    private var _marked = false
    
    def marked() = _marked

    def changeMark(): Unit = {
        _marked = !_marked
    }

    def flipped() = _flipped

    def flip(): Unit = {
        _flipped = true
    }

    override def toString(): String = {
        "x: "+x+ " y: " + y + " hasMine: " + hasMine + " flipped: " + _flipped + " marked: " + _marked
    }
}
