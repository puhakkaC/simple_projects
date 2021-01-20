'''
Created on 18.10.2019

@author: jenni
'''

paivatk = 365 / 12

def laske_isot_pienet(keskiarvo,sahkot):
    i = 0
    p = 0
    k = 0
    summa = 0
    pienet = {}
    isot = {}
    iso = 2*keskiarvo
    pieni = keskiarvo / 2
    paivasahkot = {}

    for avain in sahkot:
        summa += float(sahkot[avain])
        k += 1
        if k == 24:
            avain = avain.split()
            avain = avain[1]
            paivasahkot[avain] = summa
            k = 0
            summa = 0
    
    for avain in paivasahkot:
        sahko = paivasahkot[avain]
        
        if sahko > iso:
            i += 1
            isot[avain] = sahko
            
        if sahko < pieni and sahko != 0: 
            p += 1
            pienet[avain] = sahko
        
    if i > 0:
        print("There were", i ,"days of exceptionally high consumption:")

        for arvo in isot:
            sahko = round(isot[arvo],2)
            arvo = str(arvo)
            print("{:>13s} ({:.2f} kWh)".format(arvo,sahko))
            
    if p > 0:
        print("And", p ,"days of exceptionally low consumption:")

        for arvo in pienet:
            sahko = round(pienet[arvo],2)
            arvo = str(arvo)
            print("{:>13s} ({:.2f} kWh)".format(arvo,sahko))        
     
    if i == 0:
        print("There were 0 days of exceptionally high consumption:") 
        
    if p == 0:
        print("And 0 days of exceptionally low consumption:")    

def prosentit(sahkoh, veroh, siirtoh, yhteensa):
    sahkohp = (sahkoh / yhteensa) * 100
    verohp = (veroh / yhteensa) * 100
    siirtohp = (siirtoh / yhteensa) * 100
    
    return sahkohp, verohp, siirtohp

def sahko_siirto_vero(lista,keskiarvo):
    ELECTRICITY_PER_KWH             = 5.15     # cent / kWh
    ELECTRICITY_TAX_PER_KWH         = 2.79372  # cent / kWh
    ELECTRICITY_TRANSFER_PER_KWH    = 3.14     # cent / kWh
    
    ELECTRICITY_BASE_COST           = 0.00     # eur / month
    ELECTRICITY_TRANSFER_BASE_COST  = 5.90     # eur / month
                   
    sahkoh = (keskiarvo * paivatk) * (ELECTRICITY_PER_KWH / 100)
    siirtoh = ((keskiarvo * paivatk) * (ELECTRICITY_TRANSFER_PER_KWH / 100)) + ELECTRICITY_TRANSFER_BASE_COST
    veroh = (keskiarvo * paivatk) * (ELECTRICITY_TAX_PER_KWH / 100)
    
    yhteensa = sahkoh + siirtoh + veroh
    
    return sahkoh, veroh, siirtoh, yhteensa

def keskiarvo_kulutuksesta(lista,rivit):
    summa = 0
    k = 0
    m = 0
    paivasahkot = {}
    
    for avain in lista:
        if lista[avain] == "m":
            lista[avain]= 0
            m += 1
        summa += float(lista[avain])
        k += 1
        if k == 24:
            avain = avain.split()
            avain = avain[1]
            paivasahkot[avain] = summa
            k = 0
            summa = 0
            if m == 24:
                del paivasahkot[avain]
                rivit -= 24
            m = 0
    
    summa = 0       
    for arvo in paivasahkot:
        luku = float(paivasahkot[arvo])
        summa += luku
     
    keskiarvo = summa / (rivit / 24)
    
    return keskiarvo, paivasahkot

def main():
    s = 0
    k = 0
    h = 0
    done = False
    
    paivat = 0
    sahkot = {}
    sahkot2 = {}
    
    try:
        rivi = input("Enter the filename containing the electricity data:\n")
        tiedosto = str(rivi)
        tiedosto1 = open(tiedosto,'r')
        
        print("File read successfully!")
        tiedosto1.readline()
            
        for rivi in tiedosto1:
            rivi = rivi.rstrip()
            rivi1 = rivi.split(";")
             
            try:
                avain = str(rivi1[0])
                maara = float(rivi1[1])
                k += 1 
                sahkot[avain] = maara
                sahkot2[avain] = maara
                                
            except ValueError:
                sahkot[avain] = "m"
                sahkot2[avain]  = "m"
                s += 1     
                
            h += 1   
           
        tiedosto1.close()
 
        print("There were", k ,"lines containing valid data and", s ,"lines missing data.")
        
        keskiarvo,paivasahkot = keskiarvo_kulutuksesta(sahkot,h)   
        keskiarvo2 = round(keskiarvo,2)
        
        print("The average consumption was {:.2f} kWh / day.\n".format(keskiarvo2))
        
        sahkoh, veroh, siirtoh, yhteensa = sahko_siirto_vero(paivasahkot,keskiarvo)
        sahkohp, verohp, siirtohp = prosentit(sahkoh, veroh, siirtoh, yhteensa)
        sahkoh = round(sahkoh,2)
        veroh = round(veroh,2)
        siirtoh = round(siirtoh,2)
        yhteensa = round(yhteensa,2)
        sahkohp = round(sahkohp,0)
        verohp = round(verohp,0)
        siirtohp = round(siirtohp,0)
        
        print("Your average electricity cost breakdown:")
        print("Electricity itself: -----{:^6.2f}eur / month ( {:.0f} %)".format(sahkoh,sahkohp))
        print("Electricity tax: --------{:^6.2f}eur / month ( {:.0f} %)".format(veroh,verohp))
        print("Transfer: ---------------{:^6.2f}eur / month ( {:.0f} %)".format(siirtoh,siirtohp))
        print("Total: ------------------{:<.2f} eur / month (100 %)".format(yhteensa))

        print("")
        laske_isot_pienet(keskiarvo,sahkot)
        print("")
        
        while not done:
            try:
                rivi = input("Enter an hour of interest of the day (integer between 0 and 23):\n")
                aika = int(rivi)
                if aika < 0 or aika > 23:
                    print("The input must be an integer in the range of 0 to 23.")
                    done = False
                if aika < 24 and aika >= 0:
                    done = True    
            except ValueError:
                print("The input must be an integer in the range of 0 to 23.")
            
        s = 0
        summa = 0
        maara = 0
        for avain in sahkot2:
            s += 1
            if s == aika+1:
                if sahkot2[avain] != "m":
                    summa += float(sahkot2[avain])
                    maara += 1 
                s -= 24
         
        tuntikeskiarvo = summa / maara
        pieni = 0.3 * tuntikeskiarvo
        iso = 2 * tuntikeskiarvo
        tuntikeskiarvo = round(tuntikeskiarvo,2)
        if aika < 10:
            aika1 = "0" + str(aika) + ":00"
            aika2 = "0" + str(aika) + ":59"
        else:
            aika1 = str(aika) + ":00"
            aika2 = str(aika) + ":59"
        
        s = 0    
        print("Average consumption from {:s} to {:s} is {:.2f} kWh / hour.".format(aika1,aika2,tuntikeskiarvo))    
        
        for avain in sahkot2:
            s += 1
            if s == aika+1:
                if sahkot2[avain] != "m":
                    if sahkot2[avain] < pieni:
                        maara = sahkot2[avain]
                        avain2 = avain.split()
                        avain2 = avain2[1]
                        print("On {:>9s} you exceptionally consumed as little as {:.2f} kWh during {:s} - {:s}".format(avain2,maara,aika1,aika2))
                       
                    if sahkot2[avain] > iso:
                        maara = sahkot2[avain]
                        avain2 = avain.split()
                        avain2 = avain2[1]
                        print("On {:>9s} you exceptionally consumed as much   as {:.2f} kWh during {:s} - {:s}".format(avain2,maara,aika1,aika2))
                s -= 24
                
    except OSError:
        if ".csv" in tiedosto:
            print("Error in reading the file '{:s}'.".format(tiedosto))
        
        else:
            try:
                tiedosto = str(tiedosto + ".csv")
                tiedosto1 = open(tiedosto,'r')
                
                print("File read successfully!")
                tiedosto1.readline()
            
                for rivi in tiedosto1:
                    rivi = rivi.rstrip()
                    rivi1 = rivi.split(";")
             
                    try:
                        avain = str(rivi1[0])
                        maara = float(rivi1[1])
                        k += 1 
                        sahkot[avain] = maara
                        sahkot2[avain] = maara
                                
                    except ValueError:
                        sahkot[avain] = "m"
                        sahkot2[avain]  = "m"
                        s += 1     
                
                    h += 1   
           
                tiedosto1.close()
 
                print("There were", k ,"lines containing valid data and", s ,"lines missing data.")
        
                keskiarvo,paivasahkot = keskiarvo_kulutuksesta(sahkot,h)   
                keskiarvo2 = round(keskiarvo,2)
        
                print("The average consumption was {:.2f} kWh / day.\n".format(keskiarvo2))
        
                sahkoh, veroh, siirtoh, yhteensa = sahko_siirto_vero(paivasahkot,keskiarvo)
                sahkohp, verohp, siirtohp = prosentit(sahkoh, veroh, siirtoh, yhteensa)
                sahkoh = round(sahkoh,2)
                veroh = round(veroh,2)
                siirtoh = round(siirtoh,2)
                yhteensa = round(yhteensa,2)
                sahkohp = round(sahkohp,0)
                verohp = round(verohp,0)
                siirtohp = round(siirtohp,0)
        
                print("Your average electricity cost breakdown:")
                print("Electricity itself: -----{:^6.2f}eur / month ( {:.0f} %)".format(sahkoh,sahkohp))
                print("Electricity tax: --------{:^6.2f}eur / month ( {:.0f} %)".format(veroh,verohp))
                print("Transfer: ---------------{:^6.2f}eur / month ( {:.0f} %)".format(siirtoh,siirtohp))
                print("Total: ------------------{:<.2f} eur / month (100 %)".format(yhteensa))

                print("")
                laske_isot_pienet(keskiarvo,sahkot)
                print("")
        
                while not done:
                    try:
                        rivi = input("Enter an hour of interest of the day (integer between 0 and 23):\n")
                        aika = int(rivi)
                        if aika < 0 or aika > 23:
                            print("The input must be an integer in the range of 0 to 23.")
                            done = False
                        if aika < 24 and aika >= 0:
                            done = True    
                    except ValueError:
                        print("The input must be an integer in the range of 0 to 23.")
            
                s = 0
                summa = 0
                maara = 0
                for avain in sahkot2:
                    s += 1
                    if s == aika+1:
                        if sahkot2[avain] != "m":
                            summa += float(sahkot2[avain])
                            maara += 1 
                        s -= 24
         
                tuntikeskiarvo = summa / maara
                pieni = 0.3 * tuntikeskiarvo
                iso = 2 * tuntikeskiarvo
                tuntikeskiarvo = round(tuntikeskiarvo,2)
                if aika < 10:
                    aika1 = "0" + str(aika) + ":00"
                    aika2 = "0" + str(aika) + ":59"
                else:
                    aika1 = str(aika) + ":00"
                    aika2 = str(aika) + ":59"
        
                s = 0    
                print("Average consumption from {:s} to {:s} is {:.2f} kWh / hour.".format(aika1,aika2,tuntikeskiarvo))    
        
                for avain in sahkot2:
                    s += 1
                    if s == aika+1:
                        if sahkot2[avain] != "m":
                            if sahkot2[avain] < pieni:
                                maara = sahkot2[avain]
                                avain2 = avain.split()
                                avain2 = avain2[1]
                                print("On {:>9s} you exceptionally consumed as little as {:.2f} kWh during {:s} - {:s}".format(avain2,maara,aika1,aika2))
                       
                            if sahkot2[avain] > iso:
                                maara = sahkot2[avain]
                                avain2 = avain.split()
                                avain2 = avain2[1]
                                print("On {:>9s} you exceptionally consumed as much   as {:.2f} kWh during {:s} - {:s}".format(avain2,maara,aika1,aika2))
                        s -= 24
                
            except OSError:
                print("Error in reading the file {:s}.".format(tiedosto))
            
main()            
        
        