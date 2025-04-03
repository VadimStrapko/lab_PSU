    $(document).ready(function() {
        $('#btn').click(function() {
            $('#textBlock').fadeToggle(600); 
        });
    });

    $(document).ready(function() {
        function moveElement() {
            const size = Math.random() * 100 + 50; 
            const posX = Math.random() * ($(window).width() - size);
            const posY = Math.random() * ($(window).height() - size);

            $('#movable').css({
                left: posX + 'px',
                top: posY + 'px',
                width: size + 'px',
                height: size + 'px',
                backgroundColor: 'hsl(' + (Math.random() * 360) + ', 100%, 50%)' 
            });
        }

        setInterval(moveElement, 1000);
    });

    $(document).ready(function() {
        $('#toggleButton').click(function() {
            $('#menu').slideToggle(300); 
            $('body').toggleClass('menu-open'); 
        });
    });