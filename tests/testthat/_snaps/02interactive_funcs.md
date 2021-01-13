# hr

    <hr style="border: 0.5px solid #3b8dbc38;"/>

# textInputGroup

    <div class="row">
      <div class="col-sm-9" style="padding-right: 0; bottom: 10px">
        <div>
          <label for="id1"></label>
          <span class="text-input-clearable" style="background-color: #f5f5f5;">
            <input id="id1" type="text" value="" placeholder=""/>
            <span id="clear_input"
                     class="glyphicon glyphicon-remove"></span>
          </span>
        </div>
        <script>clearText('id1')</script>
      </div>
      <div class="col-sm-3" style="padding-left: 10px; bottom: 10px">
        <br/>
        <button id="id2" type="button" class="btn btn-default action-button">
          <i class="fa fa-paper-plane" role="presentation" aria-label="paper-plane icon"></i>
          
        </button>
      </div>
    </div>

# clearableTextInput

    <div>
      <label for="input1">This is a input box</label>
      <span class="text-input-clearable" style="background-color: #f5f5f5;">
        <input id="input1" type="text" value="" placeholder=""/>
        <span id="clear_input"
                     class="glyphicon glyphicon-remove"></span>
      </span>
    </div>
    <script>clearText('input1')</script>

# tabtile

    <h2 style="color:#17a2b8;">This title</h2>

# gallery

    <div id="id" class="col">
      <p class="text-center h2" style="color: #0275d8;">Gallery</p>
      <div class="row" style="  margin: 10px;">   <a href="a.a"  class="col-sm-4 sps-tab-link" style="right: 1px;">
         <img src="a.jpg" class="img-gallery" height=300 width=400 style="width: 100%;">
         <p class="text-center h4">a</p>
       </a></div>
    </div>

# hexLogo

    <div id="logo" class="hex-container">
      
      <svg class="hex-box" viewBox="0 0 100 115" version="1.1" xmlns="http://www.w3.org/2000/svg">
        <defs>
          <pattern id="logo-hex" patternUnits="userSpaceOnUse" height="100%" width="100%">
            <image href="log.jpg" x="-10" y="-20" height="125%" width="125%" />
          </pattern>
        </defs>
        <polygon points="50 1 95 25 95 75 50 99 5 75 5 25"fill="url(#logo-hex)" stroke="var(--primary)"stroke-width="2"/>
        
      </svg>
    </div>

# hexPanel

    <div class="row hex-panel">
      <h5 class="text-primary">panel</h5>
      <div class="hex-item"><div id="panel1" class="hex-container">
      
      <svg class="hex-box" viewBox="0 0 100 115" version="1.1" xmlns="http://www.w3.org/2000/svg">
        <defs>
          <pattern id="panel1-hex" patternUnits="userSpaceOnUse" height="100%" width="100%">
            <image href="panel.jpg" x="-10" y="-20" height="125%" width="125%" />
          </pattern>
        </defs>
        <polygon points="50 1 95 25 95 75 50 99 5 75 5 25"fill="url(#panel1-hex)" stroke="var(--primary)"stroke-width="2"/>
        
      </svg>
    </div></div>
    </div>

# hrefTab

    <div id="a" class="col">
      <p class="h4" style="color: #0275d8; text-align: left;">A list of tabs</p>
      <div><a href="" class="href-button
    sps-tab-link">a</a>
     <a href="" class="href-button
    sps-tab-link">b</a>
    </div>
    </div>

# hrefTable

    <table id="table" class="table table-hover table-href table-striped">
      <caption class="text-center h2" style="color: #0275d8;">A Table of list of tabs</caption>
      <thead>
                    <tr class="info">
                      <th>Category</th>
                      <th>Options</th>
                    </tr>
                  </thead>
      <tbody>  <tr>
        <td class="h4" style="color: #0275d8;">a</td>
        <td><a href="" class="href-button sps-tab-link">a1</a></td>
      </tr>
       <tr>
        <td class="h4" style="color: #0275d8;">b</td>
        <td><a href="" class="href-button sps-tab-link">b1</a><a href="" class="href-button sps-tab-link">b2</a></td>
      </tr>
    </tbody>
    </table>

# pgPaneUI

    <div class="tab-pane" id="thispg-pg-container">
      <div class="control-panel draggable" style="top:5%;right:2%;width:310px;height:auto;position:absolute;cursor:inherit; background-color: white; z-index:999;">
        <div class="row">
          <div class="col-sm-3"></div>
          <div class="col-sm-7">
            <h4>Example Progress</h4>
          </div>
          <div class="col-sm-2"><button class="action-button bttn bttn-simple bttn-xs bttn-primary bttn-no-outline"data-target="#thispg-pg-collapse" data-toggle="collapse"><i class="fa fa-minus"></i></button></div>
        </div>
        <div class="collapse" id="thispg-pg-collapse">
          <ul class="timeline" id="thispg-timeline">
            <li style="margin-bottom: 0;">
              <i id="a-icon" class="fa fa-times bg-red"></i>
              <div class="timeline-item">
                <h3 class="timeline-header no-border">this a</h3>
                <div class="timeline-body" style="padding: 0px;">
                  <div class="progress-group">
                    <div class="progress">
                      <div class="progress-bar progress-bar-primary progress-bar-striped" id="a-pg" role="progressbar"></div>
                    </div>
                  </div>
                </div>
              </div>
            </li>
            <li style="margin-bottom: 0;">
              <i id="b-icon" class="fa fa-times bg-red"></i>
              <div class="timeline-item">
                <h3 class="timeline-header no-border">this b</h3>
                <div class="timeline-body" style="padding: 0px;">
                  <div class="progress-group">
                    <div class="progress">
                      <div class="progress-bar progress-bar-primary progress-bar-striped" id="b-pg" role="progressbar"></div>
                    </div>
                  </div>
                </div>
              </div>
            </li>
            <li class="time-label">
              <span class="bg-orange" id="thispg-pg-label">Ready</span>
            </li>
            <div style="margin-left: 60px; margin-right: 15px;">
              <div class="progress-group">
                <div class="progress">
                  <div class="progress-bar progress-bar-primary progress-bar-striped" id="thispg-pg-all" role="progressbar"></div>
                </div>
              </div>
            </div>
          </ul>
        </div>
      </div>
      <script>$(".draggable").draggable();</script>
    </div>

# renderDesc

      <div class="desc">
        <div class="collapse desc-body" id="desc" aria-expanded="false">
         <h2>title</h2>
    
    <ol>
    <li>xxx</li>
    <li>xxx
    <code>aaa</code></li>
    </ol>
    
        </div>
    
        <a role="button" class="collapsed" data-toggle="collapse"
           href="#desc" aria-expanded="false" aria-controls="desc">
        </a>
      </div>

