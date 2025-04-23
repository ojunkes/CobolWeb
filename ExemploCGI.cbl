      $set preprocess(htmlpp) endp webserver(isapi) case reentrant(2)
      $set sourceformat"free"

      *>===================================================================================
       identification division.
       program-id.   ExemploCGI.
       author. Anderson Junkes.
      *>===================================================================================

      *>
      *>            --------------------------------------------------------
      *>                                   CRUD CGI COBOL
      *>            --------------------------------------------------------
      *>

      *>===================================================================================
       environment division.
      *>===================================================================================

       input-output section.
       file-control.

       select arqcli assign to disk "base.dat"
              organization  is indexed
              access mode   is dynamic
              record key    is arqcli-chave
              alternate record key is arqcli-nome
              file status   is ws-file-status.

      *>===================================================================================
       data division.

       fd   arqcli.
       01   reg-arqcli.
            03 arqcli-chave.
               05 arqcli-codigo                    pic 9(05).
            03 arqcli-nome                         pic x(40).
            03 arqcli-datnasc                      pic 9(08).
            03 arqcli-sexo                         pic x(01).
            03 arqcli-banco1                       pic 9(01).
            03 arqcli-banco2                       pic 9(01).
            03 arqcli-banco3                       pic 9(01).
            03 arqcli-estado                       pic x(02).

      *>===================================================================================
       working-storage section.
       78 EV-BUSCA                                 value 2.
       78 EV-GRAVAR                                value 1.
       78 EV-EXCLUIR                               value 3.
       78 EV-PESQUISAR                             value 4.

       78 EV-LISTA-FIRST                           value 81.
       78 EV-LISTA-NEXT                            value 82.
       78 EV-LISTA-PREVIOUS                        value 83.
       78 EV-LISTA-LAST                            value 84.
       78 EV-LISTA-BACK                            value 85.

       01   ws-campos-de-trabalho.
            03 ws-file-status                      pic x(02) value zeros.
            03 sel                                 pic x(15) value spaces.
            03 ws-qtde-reg-pag                     pic 9(02) value 05.
            03 ws-ctrl-botoes                      pic 9(01) value 3.
            03 ws-ind                              pic 9(05)  value zeros.
            03 ws-dir                              pic x(150) value spaces.
            03 ws-mensagem                         pic x(150) value spaces.
            03 ws-status-code                      pic x(02) comp-x.

       01   cgi-input is external-form.
            03 f-campos.
               05 f-codigo                         pic z(05) identified by "codigo".
               05 f-nome                           pic x(40) identified by "nome".
               05 f-data-nascimento                pic x(10) identified by "nascimento".
               05 f-sexo                           pic x(01) identified by "sexo".
               05 f-banco1                         pic 9(01) identified by "banco1".
               05 f-banco2                         pic 9(01) identified by "banco2".
               05 f-banco3                         pic 9(01) identified by "banco3".
               05 f-estado                         pic x(02) identified by "estado".
            03 f-controles.
               05 f-opcao                          pic 9(02) identified by "opcao".
               05 f-qtde-reg                       pic 9(02) identified by "QtdeReg".
               05 f-start-ini                      pic x(40) identified by "StartPesqI".
               05 f-start-fim                      pic x(40) identified by "StartPesqF".

       01   ws-output-var.
            03 ws-dt-nascimento                    pic 9(08).
            03 filler redefines ws-dt-nascimento.
               05 ws-dia-9                         pic 9(02).
               05 ws-mes-9                         pic 9(02).
               05 ws-ano-9                         pic 9(04).
            03 ws-data-nascimento.
               05 ws-dia-x                         pic 9(02).
               05 filler                           pic x value "/".
               05 ws-mes-x                         pic 9(02).
               05 filler                           pic x value "/".
               05 ws-ano-x                         pic 9(04).

      *>===================================================================================
       procedure division.
      *>===================================================================================
       0000-controle section.
       0000.
          perform 1000-inicializacao
          perform 2000-processamento
          perform 3000-finalizacao
          .
       0000-saida.
          stop run.


      *>===================================================================================
       1000-inicializacao section.
       1000.
           accept cgi-input

           open i-o arqcli
           if   ws-file-status <> "00"
           and  ws-file-status <> "05"
                move spaces                        to ws-mensagem
                string "Erro abertura arquivo - Status: " ws-file-status into ws-mensagem
                perform 8000-mensagem
           end-if
           .
       1000-exit.
            exit.

      *>===================================================================================
       2000-processamento section.
       2000.
          evaluate f-opcao
             when EV-BUSCA
                 perform 2100-consultar
             when EV-GRAVAR
                 perform 2200-gravar
             when EV-EXCLUIR
                 perform 2300-excluir
             when EV-PESQUISAR
                 initialize f-campos f-start-ini
                 move 81                           to f-opcao
                 perform 8000-listar
             when EV-LISTA-FIRST thru EV-LISTA-BACK
                 perform 8000-listar
          end-evaluate

          if   f-opcao <> EV-PESQUISAR and < 80
               perform 8000-tela
          end-if
          .
       2000-exit.
            exit.

      *>===================================================================================
       2100-consultar section.
       2100.
            if   f-codigo <> zeros and spaces
                 move f-codigo                     to arqcli-codigo
                 read arqcli
                   invalid key
                     initialize reg-arqcli
                   not invalid key
                     move arqcli-nome              to f-nome
                     move arqcli-datnasc           to ws-dt-nascimento
                     move arqcli-sexo              to f-sexo
                     move arqcli-banco1            to f-banco1
                     move arqcli-banco2            to f-banco2
                     move arqcli-banco3            to f-banco3
                     move arqcli-estado            to f-estado

                     move ws-dia-9                 to ws-dia-x
                     move ws-mes-9                 to ws-mes-x
                     move ws-ano-9                 to ws-ano-x
                     move ws-data-nascimento       to f-data-nascimento
                 end-read
            end-if
            .
       2100-exit.
            exit.

      *>===================================================================================
       2200-gravar section.
       2200.
            move f-codigo                          to arqcli-codigo
            move f-nome                            to arqcli-nome

            move f-data-nascimento                 to ws-data-nascimento
            move ws-dia-x                          to ws-dia-9
            move ws-mes-x                          to ws-mes-9
            move ws-ano-x                          to ws-ano-9
            move ws-dt-nascimento                  to arqcli-datnasc

            move f-sexo                            to arqcli-sexo
            move f-banco1                          to arqcli-banco1
            move f-banco2                          to arqcli-banco2
            move f-banco3                          to arqcli-banco3
            move f-estado                          to arqcli-estado
            write reg-arqcli
            if   ws-file-status <> "00" and "02"
                 rewrite reg-arqcli
            end-if
            initialize f-campos
            .
       2200-exit.
            exit.

      *>===================================================================================
       2300-excluir section.
       2300.
            move f-codigo                          to arqcli-codigo
            initialize f-campos
            delete arqcli
            .
       2300-exit.
            exit.

      *>===================================================================================
       3000-finalizacao section.
       3000.
           close arqcli
           .
       3000-exit.
            exit.

      *>===================================================================================
       8000-tela section.
       8000.
           exec html
              <FORM name=form1 method=post>
              <INPUT type=hidden name=opcao value=:f-opcao>
              <TABLE border=0>
                 <TR>
                    <TD><label>Codigo :
                    <TD><INPUT type=text name=codigo size=7 maxlength=5 value=:f-codigo>
                        <INPUT type=button name=consultar value=Busca onclick="Evento(2);">
                 <TR>
                    <TD><label>Nome :
                    <TD colspan=4><input type=text name=nome size=40 maxlength=40 value=":f-nome">
                 <TR>
                    <TD><label>Data Nascimento :
                    <td><INPUT type=text name=nascimento size=12 maxlength=10 value=:f-data-nascimento>
                 <TR>
                    <TD><label>Banco(s) :
           end-exec

           move spaces                             to sel
           if   f-banco1 = 1
                move "checked"                     to sel
           end-if

           exec html
                    <TD><INPUT type=checkbox name=banco1 value=1 :sel>
                        <label>Banco do Brasil </TD>
                    <TD><label>Sexo :
           end-exec

           move spaces                             to sel
           if   f-sexo = "M"
                move "checked"                     to sel
           end-if

           exec html
                    <TD><INPUT type=radio name=sexo value=M :sel>
                        <label>Masculino </TD>
                 <TR>
           end-exec

           move spaces                             to sel
           if   f-banco2 = 1
                move "checked"                     to sel
           end-if

           exec html
                    <TD><TD><INPUT type=checkbox name=banco2 value=1 :sel>
                            <label>Caixa </TD>
           end-exec

           move spaces                             to sel
           if   f-sexo = "F"
                move "checked"                     to sel
           end-if

           exec html
                    <TD><TD><INPUT type=radio name=sexo  value=F :sel>
                            <label>Feminino  </TD>
                 <TR>
           end-exec

           move spaces                             to sel
           if   f-banco3 = 1
                move "checked"                     to sel
           end-if

           exec html
                    <TD><TD><INPUT type=checkbox name=banco3 value=1 :sel>
                            <label>Outros </TD>
                 <TR>
                    <TD><label>Estado :
                    <TD>
                        <SELECT name=estado>
           end-exec

           move spaces                             to sel
           if   f-estado = "SC"
                move "selected"                    to sel
           end-if

           exec html
                           <OPTION :sel>SC</option>
           end-exec

           move spaces                             to sel
           if   f-estado = "PR"
                move "selected"                    to sel
           end-if

           exec html
                           <OPTION :sel>PR</option>
           end-exec

           move spaces                             to sel
           if   f-estado = "RJ"
                move "selected"                    to sel
           end-if

           exec html
                           <OPTION :sel>RS</option>
                        </SELECT>

                 <TR><TD>&nbsp;
                 <TR>
                    <TD colspan=4><center>
                       <input type=button name=gravar    value=Gravar    onclick="Evento(1);">
                       <input type=button name=excluir   value=Excluir   onclick="Evento(3);">
                       <input type=button name=pesquisar value="Listar  todos" onclick="Evento(4);">
                    </center>
              </TABLE>
            </FORM>

            <SCRIPT>
               function Evento(op){
                  document.all.opcao.value = op;
                  if(( op == 1 || op == 2 || op == 3 ) && ( document.all.codigo.value == 0 )) {
                     alert('Informe o codigo !');
                  }
                  else{
                     document.form1.submit();
                  }
               }
            end-exec

            if   arqcli-codigo equal zeros and f-opcao <> zeros
                 exec html
                    alert ('Codigo ":f-codigo" nao encontrado !');
                 end-exec
            end-if

            exec html

            </SCRIPT>
           end-exec
           .
       8000-exit.
            exit.

      *>===================================================================================
       8000-listar section.
       8000.

          exec html
             <FORM name=form2 method=post>
             <TABLE border=1 width=100%>
                <TR>
                   <TH nowrap><label>Codigo
                   <TH nowrap><label>Nome
                   <TH nowrap><label>Nascimento
                   <TH nowrap><label>Sexo
                   <TH nowrap><label>Banco do Brasil
                   <TH nowrap><label>Caixa
                   <TH nowrap><label>Outros
                   <TH nowrap><label>Estado
          end-exec

          evaluate f-opcao
             when EV-LISTA-FIRST thru EV-LISTA-NEXT
                  if   f-opcao = EV-LISTA-FIRST
                       move spaces                 to arqcli-nome
                       move 2                      to ws-ctrl-botoes
                   else
                       move f-start-fim            to arqcli-nome
                  end-if
                  start arqcli key is NOT LESS than arqcli-nome
                     invalid key
                       perform 8000-final-lista
                       exit section
                  end-start
             when EV-LISTA-PREVIOUS thru EV-LISTA-LAST
                  if   f-opcao = EV-LISTA-LAST
                       move all "z"                to arqcli-nome
                       move 1                      to ws-ctrl-botoes
                   else
                       move f-start-ini            to arqcli-nome
                  end-if
                  start arqcli key is LESS than arqcli-nome
                     invalid key
                       perform 8000-final-lista
                       exit section
                  end-start
                  read arqcli previous with ignore lock
                  perform ws-qtde-reg-pag times
                     read arqcli previous with ignore lock
                     if   ws-file-status = "10"
                          move 2                   to ws-ctrl-botoes
                     end-if
                  end-perform
          end-evaluate

          move zeros                               to f-qtde-reg

          perform until exit
             read arqcli next with ignore lock
             if   f-qtde-reg = zeros
                  move arqcli-nome                 to f-start-ini
             end-if
             if   ws-file-status <> "00" and "02" or f-qtde-reg >= ws-qtde-reg-pag
                  if   ws-file-status = "10"
                       move 1                      to ws-ctrl-botoes
                  end-if
                  move arqcli-nome                 to f-start-fim
                  exit perform
             end-if

             add 1                                 to f-qtde-reg
             move arqcli-datnasc                   to ws-dt-nascimento
             move ws-dia-9                         to ws-dia-x
             move ws-mes-9                         to ws-mes-x
             move ws-ano-9                         to ws-ano-x
             exec html
                <TR>
                    <TD nowrap><a href=anderson.exe?opcao=2&codigo=:arqcli-codigo title="Sequencia :f-qtde-reg">:arqcli-codigo</a>
                    <TD nowrap><label>:arqcli-nome
                    <TD nowrap><label>:ws-data-nascimento
                    <TD nowrap><label>:arqcli-sexo
                    <TD nowrap><label>:arqcli-banco1
                    <TD nowrap><label>:arqcli-banco2
                    <TD nowrap><label>:arqcli-banco3
                    <TD nowrap><label>:arqcli-estado
             end-exec
          end-perform
          .

       8000-final-lista.
          exec html
             </TABLE>
             <BR> &nbsp;
             <BR>
             <CENTER>
                <INPUT type=button DISABLED name=Primeiro value="<< Primeiro" onclick="EventTab(81);">
                <INPUT type=button DISABLED name=Anterior value="< Anterior"  onclick="EventTab(83);">
                <INPUT type=button DISABLED name=Proximo  value="Próximo >"   onclick="EventTab(82);">
                <INPUT type=button DISABLED name=Ultimo   value="Último >>"   onclick="EventTab(84);">
             </CENTER>
             <BR> &nbsp;
             <BR>
             <INPUT type=button name=Voltar      value=Voltar onclick='document.location.href="anderson.exe"'>
             <INPUT type=hidden name=QtdeReg     value=:f-qtde-reg>
             <INPUT type=hidden name=StartPesqI  value=:f-start-ini>
             <INPUT type=hidden name=StartPesqF  value=:f-start-fim>
             <INPUT type=hidden name=opcao       value=:f-opcao>

             <SCRIPT>
                function EventTab( opt ) {
                   document.all.opcao.value = opt;
                   document.form2.submit();
                }
             </SCRIPT>
          end-exec
          .
       8000-exit.
            exit.

      *>===================================================================================
       8000-mensagem section.
       8000.
            exec html
               <script>
                  alert( ':ws-mensagem' );
               </script>
            end-exec
            .
       8000-exit.
            exit.

      *>===================================================================================
      *>

