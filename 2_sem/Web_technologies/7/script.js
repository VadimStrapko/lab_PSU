//спросить по поводу пошаговой валидации
var username = document.getElementById('username');
var secname = document.getElementById('secname');
var number = document.getElementById('number');
var email = document.getElementById('email');
var comment = document.getElementById('comment');
var btnSell = document.getElementById('btnSell');
var btnDelete = document.getElementById('btnDelete');

const regexNumber = /^\+375(29|25|33|44)\d{7}$/; //+375291234567
const regexEmail = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/;//example@example.com

//имя
btnSell.addEventListener('click',function(){
    var errorMessage = "";

    if(username.value.trim()=='')
        errorMessage = "Это поле не может быть пустым";
        
    errorname.textContent = errorMessage;  

});

//фамилия
btnSell.addEventListener('click',function(){
    var errorMessage = "";

    if(secname.value.trim()=='')
        errorMessage = "Это поле не может быть пустым";
    
    errorsec.textContent = errorMessage;   

});

//номер
btnSell.addEventListener('click',function(){
    var errorMessage = "";

    if(!regexNumber.test(number.value))
        errorMessage = "Введите корректный номер(Например: +375291234567)";
    
    errornumber.textContent = errorMessage;   

});

//почта
btnSell.addEventListener('click',function(){
    var errorMessage = "";

    if(!regexEmail.test(email.value))
        errorMessage = "Введите корректный E-mail";
    
    erroremail.textContent = errorMessage;   

});

//комментарий
btnSell.addEventListener('click',function(){
    var errorMessage = "";

    if(comment.value.trim()=='')
        errorMessage = "Добавьте, пожалуйста, комментарий";
    
    errorcomment.textContent = errorMessage;   
    confirm("Вы уверены в отправке?");

});

btnDelete.addEventListener('click',function(){
    confirm("Вы уверены, что хотите сбросить данные?");

    var errorMessage = "";
    username.value = "";
    secname.value = "";
    number.value = "";  
    email.value = "";  
    comment.value = ""; 


});





  