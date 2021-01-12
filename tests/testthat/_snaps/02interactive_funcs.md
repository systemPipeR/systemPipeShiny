# textInputGroup func

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

