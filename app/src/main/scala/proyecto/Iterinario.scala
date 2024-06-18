package proyecto

class Itinerario() {

  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]


  def itinerarios(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los itinerarios posibles de cod1 a cod2
    def aux(codigoOrigen : String, codigoDestino: String): List[List[Vuelo]] = {
      if (codigoOrigen == codigoDestino) {
        List(List())
      } else {
        val filtroVuelosCodigo = vuelos.filter(vuelo => vuelo.Org == codigoOrigen)
        val conexionVuelosCodigo = filtroVuelosCodigo.flatMap(vuelo => aux(vuelo.Dst, codigoDestino).map(vuelo :: _))
        conexionVuelosCodigo

      }
    }
    (cod1:String, cod2:String)=> aux(cod1, cod2)
  }


  def calcularTamanio(lista: List[Vuelo], acc: Int): Int = {
    if (lista.isEmpty) {
      acc
    } else {
      calcularTamanio(lista.tail, acc + 1)
    }
  }

  def auxOrder(lista: List[(List[Vuelo], Int)]): List[(List[Vuelo], Int)] = {
    if (lista.isEmpty) {
      List()
    } else {
      val minimo = lista.foldLeft(lista.head)((aux, vuelo) => if (vuelo._2 < aux._2) vuelo else aux)
      val listaFinal = lista.filter(vuelo => vuelo != minimo)
      minimo :: auxOrder(listaFinal)

    }

  }
  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    //Recibe vuelos, una lista de vuelos y aeropuertos, una lista de aeropuertos y retorna una funcion que recibe dos strings y retorna una lista de itinerarios
    //Devuelve una función que recibe c1 y c2, códigos de aeropuertos
    //y devuelve una función que devuelve los tres (si los hay) itinerarios que minimizan el tiempo total de viaje

    def auxHora(codigo:String , hora:Int): Int = {
      val filtroAeropuertoZonaHoraria = aeropuertos.filter(aeropuerto => aeropuerto.Cod == codigo)
      val zonaHoraria = filtroAeropuertoZonaHoraria.head.GMT
      zonaHoraria match {
        case 100 => hora + 1
        case 300 => hora + 3
        case 400 => hora + 4
        case -500 => hora
        case -400 => hora + 4
        case -600 => hora
        case -700 => hora
        case -800 => hora
        case 900 => hora + 9
        case -900 => hora
      }
    }


    def aux(codigoOrigen : String, codigoDestino: String): List[List[Vuelo]] = {
      val listaItineraios = itinerarios(vuelos, aeropuertos)(codigoOrigen, codigoDestino)
      val tiempoLlegadaTotal = listaItineraios.map(
        itinerario => {
          def auxCalculoTotalViaje(vueloCalcular: List[Vuelo], tamanio: Int, acc: Int): Int = {
            if (tamanio < 1) {
              acc
            }
            else {
              val zonaHorariaOrigen = vueloCalcular.head.HS + (vueloCalcular.head.MS / 100)
              val zonaHorariaDestino = vueloCalcular.head.HL + (vueloCalcular.head.ML / 100)
              val horaSalida = auxHora(vueloCalcular.head.Org, zonaHorariaOrigen)
              val horaLlegada = auxHora(vueloCalcular.head.Dst, zonaHorariaDestino)
              val tiempoTotal = if (horaLlegada > horaSalida) horaLlegada - horaSalida else (24 + horaLlegada) - horaSalida
              auxCalculoTotalViaje(vueloCalcular.tail, tamanio - 1, acc + tiempoTotal)
            }
          }
          auxCalculoTotalViaje(itinerario, calcularTamanio(itinerario,0), 0)
        }
      )

      // lis(20, 22, 31, 53 , 19 , 12)
      // list(12,19, 20,22,31,53)
      val vueloConTiempoTotal = listaItineraios.zip(tiempoLlegadaTotal)
      val vuelosOrdenadosPorTiempoTotal = auxOrder(vueloConTiempoTotal)
      vuelosOrdenadosPorTiempoTotal.map(_._1).take(3)


    }
    (cod1:String, cod2:String)=> {aux(cod1, cod2)}
  }

  def itinerariosEscalas(vuelos:List[Vuelo], aeropuertos:List[Aeropuerto]):(String, String)=>List[List[Vuelo]]
  = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el número de escalas

    def aux(codigo1: String, codigo2: String): List[List[Vuelo]] = {
      val listaItineraios = itinerarios(vuelos, aeropuertos)(codigo1, codigo2)
      val numeroTotalEscalas = listaItineraios.map(
        itinerario => {
          def auxCalculoEscalas(vueloCalcular: List[Vuelo], tamanio: Int, acc: Int): Int = {
            if (tamanio < 1) {
              acc
            }
            else {
              val vueloEscala = vueloCalcular.head.Esc
              auxCalculoEscalas(vueloCalcular.tail, tamanio - 1, acc + vueloEscala)
            }
          }

          auxCalculoEscalas(itinerario, calcularTamanio(itinerario, 0), 0)
        }
      )
      val vueloConEscalasTotal = listaItineraios.zip(numeroTotalEscalas)
      val vuelosOrdenadosPorEscalasTotal = auxOrder(vueloConEscalasTotal)
      vuelosOrdenadosPorEscalasTotal.map(_._1).take(3)
    }
    (cod1:String, cod2:String)=> {aux(cod1, cod2)}
  }

  def itinerariosAire(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el tiempo en itinerarios
    (cod1:String, cod2:String)=> {(itinerarios(vuelos, aeropuertos)(cod1, cod2)).take(3)}
  }

  def itinerariosSalida(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String, Int, Int) => List[List[Vuelo]]= {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos y dos enteros, que es la hora de la cita
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que permiten llegar a una hora de la cita
    def aux(codigoOrigen: String, codigoDestino: String, horaLlegada: Int, MinutoLlegada: Int): List[List[Vuelo]] = {
      val listaItineraios = itinerarios(vuelos, aeropuertos)(codigoOrigen, codigoDestino)
      val mejorRutaHoraria = listaItineraios.map(
        itinerario => {
          val sumaHoraLlegada = (1000 * itinerario.last.HL) + itinerario.last.ML
          val horaLlegadaEsperada  = (1000*horaLlegada) + MinutoLlegada
          val resultado  = horaLlegadaEsperada -sumaHoraLlegada
          if (resultado  >= 0) {
            resultado
          } else {
            3000
          }
        }
      )
      val listaFinal = listaItineraios.zip(mejorRutaHoraria)
      //println(listaFinal)
      val listaOrdenada = auxOrder(listaFinal)
      //println(listaOrdenada)
      listaOrdenada.map(_._1).take(1)


    }

    (cod1: String, cod2: String, HC: Int, MC: Int) => {
      aux(cod1, cod2, HC, MC)
    }
  }
}
