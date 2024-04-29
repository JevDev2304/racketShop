#lang racket

(define cart (list
(hash 'id 1 'name "Samsung Galaxy S22" 'price 800 'description "New Samsung Galaxy S22 smartphone" 'amount 2)
  (hash 'id 2 'name "Dell XPS 15" 'price 1500 'description "New Dell XPS 15 laptop"  'amount 3)
   (hash 'id 3 'name "Google Pixel 6" 'price 600 'description "New Google Pixel 6 smartphone"  'amount 1)))
(define products
  (list
   (hash 'id 1 'name "Samsung Galaxy S22" 'price 800 'description "New Samsung Galaxy S22 smartphone" 'amount 10)
   (hash 'id 2 'name "Dell XPS 15" 'price 1500 'description "New Dell XPS 15 laptop"  'amount 10)
   (hash 'id 3 'name "Google Pixel 6" 'price 600 'description "New Google Pixel 6 smartphone"  'amount 10)
   (hash 'id 4 'name "Amazon Echo Dot" 'price 50 'description "New Amazon Echo Dot smart speaker"  'amount 10)
   (hash 'id 5 'name "Sony PlayStation 5" 'price 500 'description "New Sony PlayStation 5 game console"  'amount 10)
   (hash 'id 6 'name "Microsoft Surface Pro" 'price 1000 'description "New Microsoft Surface Pro"  'amount 10)
   (hash 'id 7 'name "Apple iPhone 13" 'price 700 'description "New Apple iPhone 13"  'amount 10)
   (hash 'id 8 'name "Nintendo Switch" 'price 300 'description "New Nintendo Switch"  'amount 10)
   (hash 'id 9 'name "HP Spectre x360" 'price 1200 'description "New HP Spectre x360"  'amount 10)
   (hash 'id 10 'name "Bose QuietComfort 35" 'price 350 'description "New Bose QuietComfort 35"  'amount 10)
   (hash 'id 11 'name "Sony WH-1000XM4" 'price 350 'description "New Sony WH-1000XM4"  'amount 10)
   (hash 'id 12 'name "Samsung Galaxy Tab S7" 'price 650 'description "New Samsung Galaxy Tab S7"  'amount 10)
   (hash 'id 13 'name "Amazon Kindle Paperwhite" 'price 130 'description "New Amazon Kindle Paperwhite"  'amount 10)
   (hash 'id 14 'name "Google Nest Hub" 'price 90 'description "New Google Nest Hub"  'amount 10)
   (hash 'id 15 'name "Apple Watch Series 7" 'price 400 'description "New Apple Watch Series 7"  'amount 10)
   (hash 'id 16 'name "Fitbit Charge 4" 'price 150 'description "New Fitbit Charge 4"  'amount 10)
   (hash 'id 17 'name "GoPro HERO9 Black" 'price 400 'description "New GoPro HERO9 Black"  'amount 10)
   (hash 'id 18 'name "DJI Mavic Air 2" 'price 800 'description "New DJI Mavic Air 2"  'amount 10)
   (hash 'id 19 'name "Canon EOS M50 Mark II" 'price 600 'description "New Canon EOS M50 Mark II"  'amount 10)
   (hash 'id 20 'name "Nikon D3500" 'price 500 'description "New Nikon D3500"  'amount 10)
   ))

(define (main)
  (displayln "\n\nWelcome to your shop")
  (displayln "Enter the operation you want:")
  (displayln "1. Get availables products")
  (displayln "2. Get cart items")
  (displayln "3. Reserve a product")
  (displayln "4. Cancel a product of my cart")
  (displayln "5. Update a product of my cart")
  (displayln "6. Checkout my Cart")
  (displayln "7. Exit")

  (define choice (read))
  (cond
    [(equal? choice 1) (getAvalaibleProducts)]
    [(equal? choice 2) (getCartItems)]
    [(equal? choice 3) (reserveProduct)]
    [(equal? choice 4) (cancelProduct)]
    [(equal? choice 5) (updateProduct)]
    [(equal? choice 6) (checkoutMyProducts)]
    [(equal? choice 7) (displayln "Ending program...")]
    [else (displayln "Invalid choice. Please enter a number between 1 and 4.") (main)]))

; 1. Get availables products
(define (getAvalaibleProducts)
  (displayln "\n==== Avalaible products ====\n")
  (getProducts products)
  )
; 2. Get cart items
(define (getCartItems)
  (displayln "\n==== Products in my Shopping Cart ====\n")
  (getProducts cart)
  )

(define (getProducts productsList)
 (for ([product productsList])
  (display "Id: ")
  (displayln (hash-ref product 'id))
  (display "Name: ")
  (displayln (hash-ref product 'name))
  (display "Price: ")
  (display (hash-ref product 'price))
  (displayln "$") 
  (display "Description: ")
  (displayln (hash-ref product 'description))
  (display "Amount: ")
  (displayln (hash-ref product 'amount)) 
           
  (newline))
  (main))

; 3. Reserve a product
(define (reserveProduct)
  (displayln "\n==== Reserve product ====\n")
  (display "Enter the id of the item you want to reserve: ")
  (define idItem (read))
  
  (when (not (idExists? idItem products))
    (begin
      (displayln "The id does not exist.")
      (main)))
  
  (display "How many units you want to reserve: ")
  (define amountItem (read))
  (unless (number? amountItem)
    (begin
      (displayln "The amount entered is not valid")
      (main)))
  
  (when (>= 0 amountItem)
    (begin
      (displayln "The amount entered must be greater than 0.")
      (main)))
  
  (define hashItems (searchProductById idItem products))
  (define hashItem (if (not (null? hashItems)) (first hashItems) (error "Product not found")))

  (if (isEnoughAmount? hashItem amountItem)
      ( begin
  
         (substractAmountOfAvalaible idItem amountItem)
         (addToShoppingCart hashItem amountItem)

         (displayln "The operation has been completed successfully.")
         )
      (displayln "There is no enough stock for your request"))
  
  (main))

; 4. Cancel a product of my cart
(define (cancelProduct)
  (displayln "\n==== Cancel product ====\n")
  (display "Enter the product id : ")
  (define id (read))
  (when (not (idExists? id cart))
    (begin
      (displayln "This id does not exist in your shopping cart.")
      (main)))
  (define hashItems (searchProductById id cart))
  (define hashItem (if (not (null? hashItems)) (first hashItems) (error "Product not found")))
  (addAmountOfAvalaible id (hash-ref hashItem 'amount))
  (set! cart (delete-item-by-id cart id))
  (displayln "The product has been removed from your shopping cart")
  (newline)

  (main))

; 5. Update a product of my cart
(define (updateProduct)
  (displayln "\n==== Update product ====\n")
  (display "Enter the product id : ")
  (define id (read))
  (display "Enter the new amount : ")
  (define newAmount (read))
  (if (<= newAmount 0)
      (begin
        (displayln "Invalid amount. The amount must be greater than 0.")
        (main))
      (let ([hashItems (searchProductById id cart)]
            [productItems (searchProductById id products)])
        (if (and (not (null? hashItems)) (not (null? productItems)))
            (let ([hashItem (first hashItems)]
                  [productItem (first productItems)])
              (if (>= (hash-ref productItem 'amount) newAmount)
                  (begin
                    (addAmountOfAvalaible id (- (hash-ref hashItem 'amount) newAmount))
                    (set! cart (update-item-by-id cart id newAmount))
                    (displayln "The operation has been completed successfully."))
                  (displayln "Not enough amount in products")))
            (displayln "Product not found"))))
  (newline)
  (main))

; 6. Checkout my Cart
(define (checkoutMyProducts)
  (displayln "\n==== Checkout Shopping Cart ====\n")
  (newline)
  (display "Checkout: ")
  (display (getCartTotal cart))
  (displayln "$" )
  (newline)
  (main))

(define (getCartTotal cart)
  (apply + (map (lambda (item) 
                  (* (hash-ref item 'price) 
                     (hash-ref item 'amount))) 
                cart)))

(define (delete-item-by-id products id)
  (filter (lambda (item) 
            (not (= (hash-ref item 'id) id))) 
          products))

(define (addAmountOfAvalaible idItem amountItem)
  (set! products
        (map (lambda (x)
               (if (equal? (hash-ref x 'id) idItem)
                   (hash-set x 'amount (+ (hash-ref x 'amount) amountItem))
                   x))
             products)))

(define (update-item-by-id products id newAmount)
  (map (lambda (x)
         (if (equal? (hash-ref x 'id) id)
             (hash-set x 'amount newAmount)
             x))
       products))

(define (searchProductById id list-hash-products)
  (filter (lambda (x) (equal? (hash-ref x 'id) id)) list-hash-products)
  )

(define (addToShoppingCart hashItem amountItem)
  (define newHashItem (hash 'id (hash-ref hashItem 'id) 'name (hash-ref hashItem 'id) 'price (hash-ref hashItem 'price) 'description (hash-ref hashItem 'description)  'amount amountItem))
  (set! cart (append cart (list newHashItem)))
)

(define (substractAmountOfAvalaible idItem amountItem)
  (set! products
        (map (lambda (x)
               (if (equal? (hash-ref x 'id) idItem)
                   (hash-set x 'amount (- (hash-ref x 'amount) amountItem))
                   x))
             products)))
 
(define (isEnoughAmount? hashItem amountItem)
  (>= (hash-ref hashItem 'amount) amountItem)
  )

(define (idExists? id hash-list)
  (cond
    [(empty? hash-list) #f]
    [(equal? (hash-ref (first hash-list) 'id) id) #t]
    [else (idExists? id (rest hash-list))]))

(main)
  