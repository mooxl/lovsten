import scala.io.Source

case class Album(title: String, date: String, artist: String, tracks: List[Track])
case class Track(title: String, length: String, rating: Int, features: List[String], writers: List[String]) 

object main {
    def main(args: Array[String]) {
        val list: List[Char] = Source.fromFile("alben.xml").mkString.toCharArray.toList
        val tokenList: List[String] = createTokenList(list, List[String](), "")
        val tokenListe: List[String] =  filter[String](
                                            polyMap[List[Char],String](
                                                partition[Char](
                                                    filter[Char](list
                                                    ,a => a != '\n' & a != '\r' & a != '\t')
                                                ,z => z == '<' | z == '>')
                                            , y => y.mkString)
                                        , x => x.isEmpty == false)
        val albumList: List[Album] = parseFile(tokenList, List[Album]())
        val thriller = albumList(0)
        /* println(thriller) */
        /* println(map[Album](albumList, Album => Album.copy(title = Album.title.toUpperCase()))) */
        /* println(map[Album](albumList, Album => {Album.copy(title = Album.title.toUpperCase()); Album.copy(tracks = map[Track](Album.tracks,Album => Album.copy(Album.title.toUpperCase())))})) */
        /* println(polyMap[Track,String](thriller.tracks, Track => Track.length)) */
        /* println(filter[Track,Track](thriller.tracks, Track => Track.rating > 4)) */
        /* println(polyMap[Track,String](filter[Track,Track](thriller.tracks, Track => Track.writers(0) == "Rod Temperton"), Track => Track.title)) */
        /* println(partition[Track](thriller.tracks, Track => Track.title == "Thriller")) */
        /* println(tokenListe) */
        /* println(vierA(_*_,x=>x,2,3)) */
        /* println(range(2,3).map(x=>x).reduceLeft(_+_)) */

    }
    
    def map[A](input_list: List[A], func: A=>A) : List[A] = input_list match {
        case Nil => List[A]()
        case x::xs => func(x) :: map(xs,func)
    }

    def polyMap[A,B](input_list: List[A], func: A=>B) : List[B] = input_list match {
        case Nil => List[B]()
        case x::xs => func(x) :: polyMap(xs,func)
    }

    def filter[A](input_list: List[A], condition: A=>Boolean) : List[A] = input_list match {
        case Nil => List[A]()
        case x::xs =>  condition(x) match {
            case true => x :: filter(xs,condition)
            case false => filter(xs,condition)
        } 
    }

    def partition[A](input_list: List[A], condition: A=>Boolean) : List[List[A]] = input_list match {
        case Nil => List[A]() :: List[List[A]]()
        case x::xs =>  condition(x) match {
            case true => List[A]() :: partition[A](xs,condition) 
            case false => val y = partition(xs,condition); (List[A](x) ::: y.head)::y.tail
        }
    } 
    
    def vierA(conc: (Int,Int) => Int, f: Int => Int, start: Int, end: Int) : Int = 
        if (start == end) end  else conc(f(start),vierA(conc,f,start+1,end)) 

    def range(x:Int,y:Int) : List[Int] = if (x>y) Nil else x::range(x+1,y)

    def createTokenList(list:List[Char], tokenList:List[String], token:String) : List[String] = list match {
        case Nil => return tokenList
        case '<'::xs => token match {
            case "" => return createTokenList(xs,tokenList,token)
            case _ => return createTokenList(xs,tokenList :+ token,"")
        }  
        case '>'::xs => token match {
            case "" => return createTokenList(xs,tokenList,token)
            case _ =>  return createTokenList(xs,tokenList :+ token,"")
        }
        case '\n'::xs => return createTokenList(xs,tokenList,token)
        case '\t'::xs => return createTokenList(xs,tokenList,token)
        case '\r'::xs => return createTokenList(xs,tokenList,token)
        case x::xs => return createTokenList(xs,tokenList,token+x)
    } 

    def parseFile(tokenList:List[String]) : List[Album] =
    {
        return parseFile(tokenList, List[Album]())
    }

    def parseFile(tokenList:List[String], albumList:List[Album]) : List[Album] = tokenList match {
        case Nil => return albumList
        case "album"::xs => val(tokenListRest, album) = createAlbum(xs, Album("","","",List[Track]())); 
                            return parseFile(tokenListRest, albumList :+ album)
    } 

    def createAlbum(tokenList:List[String], album:Album) : (List[String], Album) = tokenList match {
        case "/album"::xs => return (xs, album)
        case "track"::xs => val (tokenListRest, track) = createTrack(xs, Track("","",0,List[String](),List[String]())); 
                            return createAlbum(tokenListRest, album.copy(tracks = album.tracks :+ track))
        case "artist"::artist::_::xs => return createAlbum(xs, album.copy(artist = artist))
        case "title"::title::_::xs => return createAlbum(xs, album.copy(title = title))
        case "date"::date::_::xs => return createAlbum(xs, album.copy(date = date))
    }

    def createTrack(tokenList:List[String], track:Track)  : (List[String], Track) = tokenList match {
        case "/track"::xs => return (xs, track)
        case "title"::title::xs => return createTrack(xs.tail, track.copy(title = title))
        case "length"::length::xs => return createTrack(xs.tail, track.copy(length = length))
        case "rating"::rating::xs => return createTrack(xs.tail, track.copy(rating = rating.toInt))
        case "feature"::feature::xs => return createTrack(xs.tail, track.copy(features = track.features :+ feature))
        case "writing"::writer::xs => return createTrack(xs.tail, track.copy(writers = track.writers :+ writer))
    }
}

